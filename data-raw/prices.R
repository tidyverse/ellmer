library(dplyr)
library(tidyr)
library(stringr)

litellm_url <- "https://raw.githubusercontent.com/BerriAI/litellm/refs/heads/main/model_prices_and_context_window.json"
litellm_prices <- jsonlite::read_json(litellm_url)

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
    input = input * 1e6,
    output = output * 1e6,
    cached_input = cached_input * 1e6,
    model = stringr::str_remove(model, paste0(provider, "/"))
  ) |>
  filter(input > 0 | output > 0)
all_prices |> count(provider, sort = TRUE)

# fmt: skip
provider_lookup <- tribble(
  ~litellm_provider, ~provider,
  "openai", "OpenAI",
  "anthropic", "Anthropic",
  "gemini", "Google/Gemini",
  "vertex_ai-language-models", "Google/Vertex",
  "openrouter", "OpenRouter",
  "azure", "Azure/OpenAI",
  "bedrock", "AWS/Bedrock",
  "mistral", "Mistral",
)

prices <- all_prices |>
  inner_join(provider_lookup, join_by(provider == litellm_provider)) |>
  mutate(provider = provider.y, provider.y = NULL)

# prices |> View()

usethis::use_data(prices, overwrite = TRUE, internal = TRUE)

# Output JSON for ingestion into `chatlas`
jsonlite::write_json(prices, "data-raw/prices.json", pretty = TRUE)
