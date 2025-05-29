library(rvest)
library(httr2)
library(dplyr)

# OpenAI -----------------------------------------------------------------------

# Can't download with httr2/rvest: {curl} doesn't support brotli encoding
# Can't download with curl: as page requires javascript
# Can't download with safari: saved page doesn't contain any content
# So used ChatGPT with pasted HTML at
# https://chatgpt.com/share/67ed88c3-3b10-8009-a4e5-b62fc99f3d26

# https://platform.openai.com/docs/pricing
openai <- readr::read_csv("data-raw/openai.csv")

# Anthropic --------------------------------------------------------------------

# Same problem as OpenAI website so do it BY FUCKING HAND

# https://www.anthropic.com/pricing

# fmt: skip
anthropic <- tribble(
  ~model, ~cached_input, ~input, ~output,
  "claude-opus-4",1.50,15,75,
  "claude-sonnet-4",0.3,3,15,
  "claude-3-7-sonnet",0.3,3,15,
  "claude-3-5-sonnet",0.3,3,15,
  "claude-3-5-haiku",0.08,0.80,4,
  "claude-3-opus",1.5,15,75,
  "claude-3-haiku",0.03,0.25,1.25
)

# Gemini -----------------------------------------------------------------------

# fmt: skip
gemini <- tribble(
  ~model, ~cached_input, ~input, ~output,
  "gemini-2.0-flash",0.025,0.10,0.40,
  "gemini-2.0-flash-lite",NA,0.075,0.30,
  "gemini-1.5-flash",NA,0.3,0.075
)

prices <- bind_rows(
  openai |> mutate(provider = "OpenAI", .before = 1),
  anthropic |> mutate(provider = "Anthropic"),
  gemini |> mutate(provider = "Google/Gemini")
)

usethis::use_data(prices, overwrite = TRUE, internal = TRUE)
