library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(stringr)

litellm_url <- "https://raw.githubusercontent.com/BerriAI/litellm/refs/heads/main/model_prices_and_context_window.json"

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
  "mistral",                   "Mistral",
  "groq",                      "Groq",
)

prices <- all_prices |>
  inner_join(provider_lookup, join_by(provider == litellm_provider)) |>
  mutate(provider = provider.y, provider.y = NULL) |>
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

prices <- bind_rows(prices, posit_claude_prices, posit_other_prices) |>
  arrange(provider, model, variant)

cli::cli_progress_done()

# --- sanity checks -----------------------------------------------------------

cli::cli_alert_info("Rows: {nrow(prices)}")
cli::cli_alert_info("Providers: {n_distinct(prices$provider)}")

stopifnot(
  "Expected at least 500 rows" = nrow(prices) >= 500,
  "Expected 10 providers" = n_distinct(prices$provider) >= 10
)

# --- schema validation -------------------------------------------------------

cli::cli_progress_step("Validating schema")
prices_json <- jsonlite::toJSON(prices, pretty = TRUE)
valid <- jsonvalidate::json_validate(
  prices_json,
  "data-raw/prices.schema.json",
  engine = "ajv",
  error = TRUE
)
cli::cli_progress_done()

# --- write outputs -----------------------------------------------------------

jsonlite::write_json(prices, "data-raw/prices.json", pretty = TRUE)
usethis::use_data(prices, overwrite = TRUE, internal = TRUE)
