test_that("parallel_chat_structured robustness to data.frame conversion", {
  vcr::local_cassette("parallel-data-conversion")

  # Minimal nested schema that previously triggered conversion issues
  dimension_score <- type_object(
    score = type_integer(),
    evidence = type_string()
  )
  policy_scores <- type_object(
    Party = type_string(),
    economic = dimension_score,
    social = dimension_score
  )

  prompts <- list(
    "Party A manifesto: invest in services, progressive social policies.",
    "Party B manifesto: cut taxes, conservative social policies.",
    "Party C manifesto: balanced budget, centrist social policies."
  )

  chat <- chat_openai_test()

  # convert = TRUE returns a flattened data frame with tokens and cost
  df <- parallel_chat_structured(
    chat = chat,
    prompts = prompts,
    type = policy_scores,
    include_tokens = TRUE,
    include_cost = TRUE,
    convert = TRUE
  )
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), length(prompts))
  expect_true(all(
    c(
      "Party",
      "economic.score",
      "economic.evidence",
      "social.score",
      "social.evidence"
    ) %in%
      names(df)
  ))
  expect_true(all(
    c(
      "input_tokens",
      "output_tokens",
      "cached_input_tokens",
      "cost"
    ) %in%
      names(df)
  ))

  # Basic value checks: types and token non-negativity
  expect_type(df$`economic.score`, "integer")
  expect_type(df$`social.score`, "integer")
  expect_type(df$`economic.evidence`, "character")
  expect_type(df$`social.evidence`, "character")
  expect_true(all(is.na(df$`economic.evidence`) | nzchar(df$`economic.evidence`)))
  expect_true(all(is.na(df$`social.evidence`) | nzchar(df$`social.evidence`)))
  expect_true(all(df$input_tokens >= 0))
  expect_true(all(df$output_tokens >= 0))

  # convert = FALSE returns list with same length and expected structure
  lst <- parallel_chat_structured(
    chat = chat,
    prompts = prompts,
    type = policy_scores,
    convert = FALSE
  )
  expect_type(lst, "list")
  expect_length(lst, length(prompts))
  expect_true(all(c("Party", "economic", "social") %in% names(lst[[1]])))
  expect_true(all(c("score", "evidence") %in% names(lst[[1]]$economic)))
  expect_true(all(c("score", "evidence") %in% names(lst[[1]]$social)))
})

test_that("multi_convert handles double token counts (Gemini-like)", {
  # Minimal nested schema
  dim_score <- type_object(score = type_integer(), evidence = type_string())
  schema <- type_object(Party = type_string(), economic = dim_score, social = dim_score)

  # Provider resembling Gemini (name/model only; cost may be NA which is fine)
  provider <- test_provider(name = "Google/Gemini", model = "gemini-2.5-pro")

  # Two assistant turns with JSON content matching schema and double token counts
  j1 <- list(Party = "A", economic = list(score = 1L, evidence = "e1"), social = list(score = 2L, evidence = "s1"))
  j2 <- list(Party = "B", economic = list(score = 3L, evidence = "e2"), social = list(score = 4L, evidence = "s2"))

  t1 <- Turn("assistant", contents = list(ContentJson(j1)), tokens = c(10, 5, 0) + 0.0) # doubles
  t2 <- Turn("assistant", contents = list(ContentJson(j2)), tokens = c(20, 6, 1) + 0.0) # doubles

  out <- multi_convert(
    provider = provider,
    turns = list(t1, t2),
    type = schema,
    convert = TRUE,
    include_tokens = TRUE,
    include_cost = TRUE
  )

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)
  expect_true(all(c(
    "Party", "economic.score", "economic.evidence", "social.score", "social.evidence",
    "input_tokens", "output_tokens", "cached_input_tokens", "cost"
  ) %in% names(out)))
  # Token columns should be integer-coercible and non-negative
  expect_true(all(out$input_tokens >= 0 & out$output_tokens >= 0 & out$cached_input_tokens >= 0))
})

test_that("convert_from_type flattens nested object columns", {
  dim_score <- type_object(
    score = type_integer(),
    evidence = type_string()
  )
  schema <- type_object(
    Party = type_string(),
    economic = dim_score
  )
  x <- list(
    list(Party = "A", economic = list(score = 1L, evidence = "e1")),
    list(Party = "B", economic = list(score = 2L, evidence = "e2"))
  )
  out <- convert_from_type(x, type_array(schema))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)
  expect_setequal(names(out), c("Party", "economic.score", "economic.evidence"))
  expect_identical(out$Party, c("A", "B"))
  expect_identical(out$`economic.score`, c(1L, 2L))
  expect_identical(out$`economic.evidence`, c("e1", "e2"))
})

test_that("convert_from_type flattens multiple nested object columns", {
  dim_score <- type_object(score = type_integer(), evidence = type_string())
  schema <- type_object(
    Party = type_string(),
    economic = dim_score,
    social = dim_score
  )
  x <- list(
    list(Party = "A", economic = list(score = 1L, evidence = "e1"), social = list(score = 3L, evidence = "s1")),
    list(Party = "B", economic = list(score = 2L, evidence = "e2"), social = list(score = 4L, evidence = "s2"))
  )
  out <- convert_from_type(x, type_array(schema))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)
  expect_setequal(
    names(out),
    c("Party", "economic.score", "economic.evidence", "social.score", "social.evidence")
  )
  expect_identical(out$Party, c("A", "B"))
  expect_identical(out$`economic.score`, c(1L, 2L))
  expect_identical(out$`economic.evidence`, c("e1", "e2"))
  expect_identical(out$`social.score`, c(3L, 4L))
  expect_identical(out$`social.evidence`, c("s1", "s2"))
})

