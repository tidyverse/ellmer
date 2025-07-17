library(dplyr)
library(tidyr)

litellm_url <- "https://raw.githubusercontent.com/BerriAI/litellm/refs/heads/main/model_prices_and_context_window.json"
litellm_prices <- jsonlite::read_json(litellm_url)

df <- tibble::enframe(litellm_prices, "model", "data")
all_prices <- df |>
  hoist(
    data,
    provider = "litellm_provider",
    cached_input = "cache_read_input_token_cost",
    input = "input_cost_per_token",
    output = "output_cost_per_token"
  ) |>
  filter(model != "sample_spec") |>
  mutate(
    input = input * 1e6,
    output = output * 1e6,
    cached_input = cached_input * 1e6,
    data = NULL,
    model = stringr::str_remove(model, paste0(provider, "/"))
  ) |>
  relocate(provider) |>
  arrange(provider, model) |>
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
