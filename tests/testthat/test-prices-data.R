# Pin the provider contract independently of the updater's lookup.
EXPECTED_PRICE_PROVIDERS <- c(
  "Anthropic",
  "AWS/Bedrock",
  "Azure/OpenAI",
  "Google/Gemini",
  "Google/Vertex",
  "Groq",
  "Mistral",
  "OpenAI",
  "Posit"
)

price_raw_file <- function(name) {
  path <- testthat::test_path("../../data-raw", name)
  if (!file.exists(path)) {
    testthat::skip(paste0(
      "data-raw/",
      name,
      " is not available in this build (.Rbuildignore excludes data-raw from installed packages)"
    ))
  }
  path
}

source_price_providers <- function() {
  env <- new.env(parent = globalenv())
  source(price_raw_file("price-providers.R"), local = env)
  env$validate_price_providers
}

# Bundled data: always available ----------------------------------------------

test_that("bundled prices contain the expected providers and no OpenRouter", {
  expect_equal(sort(unique(prices$provider)), sort(EXPECTED_PRICE_PROVIDERS))
  expect_gte(nrow(prices), 1000)
})

# validate_price_providers() (dev-only) ---------------------------------------

test_that("validate_price_providers() accepts a valid provider set", {
  validate_price_providers <- source_price_providers()

  df <- data.frame(
    provider = rep(EXPECTED_PRICE_PROVIDERS, each = 2),
    model = "m",
    variant = ""
  )

  expect_no_error(validate_price_providers(df, EXPECTED_PRICE_PROVIDERS))
  expect_equal(
    validate_price_providers(df, EXPECTED_PRICE_PROVIDERS),
    sort(EXPECTED_PRICE_PROVIDERS)
  )
})

test_that("validate_price_providers() names missing providers", {
  validate_price_providers <- source_price_providers()

  df <- data.frame(
    provider = c("Anthropic", "OpenAI"),
    model = "m",
    variant = ""
  )

  expect_error(
    validate_price_providers(df, EXPECTED_PRICE_PROVIDERS),
    class = "ellmer_price_providers_invalid"
  )
  expect_snapshot(
    error = TRUE,
    validate_price_providers(df, EXPECTED_PRICE_PROVIDERS)
  )
})

test_that("validate_price_providers() detects a same-count substitution", {
  # All nine providers present, but one is swapped, so the count alone
  # cannot pass: "OpenAI" is missing and "OpenRouter" is unexpected.
  validate_price_providers <- source_price_providers()

  swapped <- EXPECTED_PRICE_PROVIDERS
  swapped[swapped == "OpenAI"] <- "OpenRouter"
  df <- data.frame(provider = swapped, model = "m", variant = "")

  expect_error(
    validate_price_providers(df, EXPECTED_PRICE_PROVIDERS),
    class = "ellmer_price_providers_invalid"
  )
  expect_snapshot(
    error = TRUE,
    validate_price_providers(df, EXPECTED_PRICE_PROVIDERS)
  )
})

test_that("validate_price_providers() names unexpected providers", {
  validate_price_providers <- source_price_providers()

  df <- data.frame(
    provider = c(EXPECTED_PRICE_PROVIDERS, "OpenRouter"),
    model = "m",
    variant = ""
  )

  expect_error(
    validate_price_providers(df, EXPECTED_PRICE_PROVIDERS),
    class = "ellmer_price_providers_invalid"
  )
  expect_snapshot(
    error = TRUE,
    validate_price_providers(df, EXPECTED_PRICE_PROVIDERS)
  )
})

test_that("validate_price_providers() accepts a custom expected set", {
  validate_price_providers <- source_price_providers()

  df <- data.frame(provider = "OpenRouter", model = "m", variant = "")
  expect_no_error(
    validate_price_providers(df, expected = "OpenRouter")
  )
})

# Bundled data and JSON snapshot agreement (dev-only) -------------------------

test_that("bundled prices agree with the committed JSON snapshot", {
  json_path <- price_raw_file("prices.json")
  json <- jsonlite::fromJSON(json_path, simplifyVector = TRUE)

  expect_setequal(unique(json$data$provider), unique(prices$provider))

  expect_identical(
    as.integer(json$schema_version),
    as.integer(attr(prices, "schema_version"))
  )
  expect_identical(
    as.character(json$updated_at),
    as.character(attr(prices, "updated_at"))
  )
  expect_equal(nrow(json$data), nrow(prices))
})

test_that("JSON snapshot provider enum still allows historical providers", {
  # Historical snapshots remain valid; the updater enforces the current set.
  schema_path <- price_raw_file("prices.schema.json")

  schema <- jsonlite::fromJSON(schema_path, simplifyVector = FALSE)
  enum <- unlist(
    schema$properties$data$items$properties$provider$enum
  )
  expect_contains(enum, "OpenRouter")
})

# Cached snapshots with removed rows ------------------------------------------

test_that("cached snapshot containing OpenRouter rows stays compatible", {
  # Removing rows is not a schema change.
  local_prices()
  cache_path <- local_prices_cache()

  openrouter <- prices[1, ]
  openrouter$provider <- "OpenRouter"
  cached <- write_prices_cache(cache_path, rbind(prices, openrouter))

  expect_equal(prices_get(), cached)
})
