library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(stringr)

# --- constants --------------------------------------------------------------

# Bump SCHEMA_VERSION for ANY change to the column structure: adding, removing,
# renaming, or changing the type of a column. Updating row values (new models,
# new prices) does not require a bump. When bumped, set min_ellmer_version to
# the release that introduces the new schema so users know which version to
# install. Update both values together whenever the schema changes.
SCHEMA_VERSION <- 1L
MIN_ELLMER_VERSION <- "0.5.0"

litellm_url <- "https://raw.githubusercontent.com/BerriAI/litellm/refs/heads/main/model_prices_and_context_window.json"


# --- fetch data --------------------------------------------------------------

cli::cli_progress_step("Fetching litellm prices")
litellm_prices <- jsonlite::read_json(litellm_url)

cli::cli_progress_step("Transforming prices")
df <- tibble::enframe(litellm_prices, "model", "data")

all_prices <- df |>
  filter(model != "sample_spec") |>
  unnest_wider(data) |>
  select(
    provider = "litellm_provider",
    model,
    starts_with("input_cost_per_token"),
    starts_with("output_cost_per_token"),
    starts_with("cache_read_input_token_cost")
  ) |>
  rename_with(\(x) {
    x |>
      str_replace("input_cost_per_token", "input") |>
      str_replace("output_cost_per_token", "output") |>
      str_replace("cache_read_input_token_cost", "cached_input")
  }) |>
  pivot_longer(
    !(provider:model),
    names_to = c(".value", "variant"),
    names_pattern = "(input|output|cached_input)_?(.*)",
    values_drop_na = TRUE
  ) |>
  arrange(provider, model, variant) |>
  mutate(
    input = round(input * 1e6, digits = 6),
    output = round(output * 1e6, digits = 6),
    cached_input = round(cached_input * 1e6, digits = 6),
    model = stringr::str_remove(model, paste0(provider, "/"))
  ) |>
  filter(input > 0 | output > 0)

# fmt: skip
provider_lookup <- tribble(
  ~litellm_provider, ~provider,
  "openai",                    "OpenAI",
  "anthropic",                 "Anthropic",

  "gemini",                    "Google/Gemini",
  "vertex_ai-language-models", "Google/Vertex",
  "openrouter",                "OpenRouter",
  "azure",                     "Azure/OpenAI",
  "bedrock",                   "AWS/Bedrock",
  "bedrock_converse",          "AWS/Bedrock",
  "bedrock_mantle",            "AWS/Bedrock",
  "mistral",                   "Mistral",
  "groq",                      "Groq",
)

prices <- all_prices |>
  inner_join(provider_lookup, join_by(provider == litellm_provider)) |>
  mutate(provider = provider.y, provider.y = NULL) |>
  distinct(provider, model, variant, .keep_all = TRUE) |>
  arrange(provider, model, variant)

# Derive Posit AI pricing from lab rates, adjusted by the service's markup.
# Gemma is served separately and entered manually.
posit_claude_models <- c(
  "claude-fable-5",
  "claude-opus-4-8",
  "claude-opus-4-7",
  "claude-opus-4-6",
  "claude-opus-4-5",
  "claude-sonnet-4-6",
  "claude-sonnet-4-5",
  "claude-haiku-4-5"
)

posit_claude_prices <- prices |>
  filter(provider == "Anthropic", model %in% posit_claude_models) |>
  mutate(
    provider = "Posit",
    across(c(input, output, cached_input), \(x) round(x * 1.1, digits = 6))
  )

# fmt: skip
posit_other_prices <- tibble::tribble(
  ~model,                      ~input, ~output, ~cached_input,
  "google/gemma-4-26B-A4B-it", 0.30,   1.50,    0.03,
) |>
  mutate(provider = "Posit", variant = "") |>
  select(provider, model, variant, input, output, cached_input)

prices <- bind_rows(
  prices,
  posit_claude_prices,
  posit_other_prices
) |>
  arrange(provider, model, variant)

cli::cli_progress_done()

# --- sanity checks -----------------------------------------------------------

cli::cli_alert_info("Rows: {nrow(prices)}")
cli::cli_alert_info("Providers: {n_distinct(prices$provider)}")

stopifnot(
  "Expected at least 1000 rows" = nrow(prices) >= 1000,
  "Expected 10 providers" = n_distinct(prices$provider) >= 10
)

# --- snapshot metadata -------------------------------------------------------
timestamp_now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

previous <- tryCatch(
  jsonlite::read_json("data-raw/prices.json", simplifyVector = TRUE),
  error = function(cnd) NULL
)

to_json <- function(x) {
  jsonlite::toJSON(
    x,
    dataframe = "rows",
    auto_unbox = TRUE,
    digits = 6,
    pretty = TRUE
  )
}

prices_unchanged <- is.list(previous) &&
  identical(as.integer(previous$schema_version), SCHEMA_VERSION) &&
  identical(to_json(prices), to_json(previous$data))

attr(prices, "schema_version") <- SCHEMA_VERSION
attr(prices, "updated_at") <- timestamp_now

prices_envelope <- list(
  # schema_version and min_ellmer_version protect old ellmer versions from
  # using incompatible pricing data when the pricing data structure is updated
  schema_version = SCHEMA_VERSION,
  min_ellmer_version = MIN_ELLMER_VERSION,
  updated_at = timestamp_now,
  data = prices
)

# --- schema validation -------------------------------------------------------

cli::cli_progress_step("Validating schema")
prices_json <- to_json(prices_envelope)
valid <- jsonvalidate::json_validate(
  prices_json,
  "data-raw/prices.schema.json",
  engine = "ajv",
  error = TRUE
)
cli::cli_progress_done()

# --- write outputs -----------------------------------------------------------

if (prices_unchanged) {
  cli::cli_alert_success("Pricing data has not changed since the last update")
} else {
  writeLines(to_json(prices_envelope), "data-raw/prices.json")
  usethis::use_data(prices, overwrite = TRUE, internal = TRUE)
}
