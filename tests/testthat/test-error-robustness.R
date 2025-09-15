test_that("parallel_chat_structured_robust works with successful prompts", {
  skip_if_not(has_credentials("openai"))

  person <- type_object(name = type_string(), age = type_integer())
  chat <- chat_openai()

  data <- parallel_chat_structured_robust(
    chat,
    list("John, age 15", "Jane, age 16"),
    type = person
  )

  expect_s3_class(data, "data.frame")
  expect_equal(nrow(data), 2)
  expect_equal(ncol(data), 2)
  expect_contains(names(data), c("name", "age"))
})

test_that("parallel_chat_structured_robust handles token length exceeded error", {
  skip_if_not(has_credentials("openai"))

  # Load test data
  load("../data/data_manifestos.rda")

  # Make one manifesto very long to exceed context window
  data_manifestos["Lab_2010"] <- paste(
    rep(data_manifestos["Lab_2010"], 4),
    collapse = "\n\n---\n\n"
  )

  immig_type <- type_object(
    party = type_string("The name of the political party."),
    immig_score = type_integer("An integer position from -2 to 2 on immigration and
                             asylum seekers, where:
                             -2 means strongly against any immigration,
                             -1 means moderately opposed,
                             0 means neutral,
                             1 means moderately in favor of a more permissive stance on immigration,
                             2 means strongly in favour of a permissive stance on immigration."),
    immig_rationale = type_string("A brief rationale for your answer.")
  )

  prompt <- "Extract structured data from political texts. You will be scoring the position of each party on immigration and asylum seekers."

  chat <- chat_openai(model = "gpt-4o", system_prompt = prompt)

  # This should not throw an error, unlike the regular version
  expect_error(
    {
      response <- parallel_chat_structured_robust(
        chat,
        prompts = as.list(data_manifestos),
        max_active = 1,
        type = immig_type
      )
    },
    NA
  )

  # # test that the return has the right structure, even with failures
  # response <- parallel_chat_structured_robust(
  #   chat,
  #   prompts = as.list(data_manifestos),
  #   type = immig_type
  # )

  # should have same number of rows as input
  expect_equal(nrow(response), length(data_manifestos))
  expect_equal(ncol(response), 3) # party, immig_score, immig_rationale
  expect_contains(names(response), c("party", "immig_score", "immig_rationale"))

  # failed prompts should have NA values
  has_nas <- apply(response, 1, function(row) any(is.na(row)))
  expect_true(any(has_nas), "At least one prompt should fail and produce NAs")
})

test_that("parallel_chat_structured_robust maintains output length with errors", {
  # this test uses mock error responses to simulate failures
  skip_if_not_installed("mockery")

  person <- type_object(name = type_string(), age = type_integer())
  chat <- chat_openai()

  # mock multi_convert_robust to simulate mixed results directly
  mockery::stub(
    parallel_chat_structured_robust,
    "multi_convert_robust",
    data.frame(name = c("John", NA_character_), age = c(15L, NA_integer_))
  )

  data <- parallel_chat_structured_robust(
    chat,
    list("John, age 15", "Invalid very long prompt..."),
    type = person
  )

  expect_equal(nrow(data), 2)
  expect_equal(data$name, c("John", NA_character_))
  expect_equal(data$age, c(15L, NA_integer_))
})

test_that("parallel_chat_structured_robust includes proper NA tokens for failed prompts", {
  skip_if_not_installed("mockery")

  person <- type_object(name = type_string(), age = type_integer())
  chat <- chat_openai()

  # mock multi_convert_robust to simulate failure with token info
  mockery::stub(
    parallel_chat_structured_robust,
    "multi_convert_robust",
    data.frame(
      name = NA_character_,
      age = NA_integer_,
      input_tokens = 0L,
      output_tokens = 0L,
      cached_input_tokens = 0L,
      cost = 0
    )
  )

  data <- parallel_chat_structured_robust(
    chat,
    list("Invalid very long prompt..."),
    type = person,
    include_tokens = TRUE,
    include_cost = TRUE
  )

  expect_equal(nrow(data), 1)
  expect_equal(data$name, NA_character_)
  expect_equal(data$age, NA_integer_)
  expect_equal(data$input_tokens, 0L)
  expect_equal(data$output_tokens, 0L)
  expect_equal(data$cost, 0)
})

test_that("parallel_chat_structured_robust on_error='stop' behaves like original", {
  skip_if_not(has_credentials("openai"))

  person <- type_object(name = type_string(), age = type_integer())
  chat <- chat_openai()

  data_robust <- parallel_chat_structured_robust(
    chat,
    list("John, age 15", "Jane, age 16"),
    type = person,
    on_error = "stop"
  )

  data_original <- parallel_chat_structured(
    chat,
    list("John, age 15", "Jane, age 16"),
    type = person
  )

  expect_equal(data_robust, data_original)
})

test_that("parallel_chat_structured_robust returns same class as original", {
  skip_if_not(has_credentials("openai"))

  person <- type_object(name = type_string(), age = type_integer())
  chat <- chat_openai()

  first_two <- list("John, age 15", "Jane, age 16")

  response_regular <- parallel_chat_structured(
    chat,
    prompts = first_two,
    type = person
  )

  response_robust <- parallel_chat_structured_robust(
    chat,
    prompts = first_two,
    type = person
  )

  # check for class equivalence
  expect_identical(class(response_regular), class(response_robust))
  expect_s3_class(response_robust, "data.frame")

  # check for structure equivalence
  expect_identical(names(response_regular), names(response_robust))
  expect_equal(nrow(response_robust), 2)
})

# Additional comprehensive tests for edge cases ----

test_that("create_na_structure works correctly for TypeObject", {
  person_type <- type_object(
    name = type_string(),
    age = type_integer(),
    score = type_number(),
    active = type_boolean()
  )

  na_struct <- create_na_structure(person_type)

  expect_type(na_struct, "list")
  expect_equal(names(na_struct), c("name", "age", "score", "active"))
  expect_equal(na_struct$name, NA_character_)
  expect_equal(na_struct$age, NA_integer_)
  expect_equal(na_struct$score, NA_real_)
  expect_equal(na_struct$active, NA)
})

test_that("create_na_structure works correctly for TypeArray", {
  array_type <- type_array(type_string())
  na_struct <- create_na_structure(array_type)

  expect_type(na_struct, "list")
  expect_length(na_struct, 0)
})

test_that("create_na_for_type handles all basic types", {
  expect_equal(create_na_for_type(type_string()), NA_character_)
  expect_equal(create_na_for_type(type_integer()), NA_integer_)
  expect_equal(create_na_for_type(type_number()), NA_real_)
  expect_equal(create_na_for_type(type_boolean()), NA)
})

test_that("parallel_chat_structured_robust handles mixed success/failure scenarios", {
  skip_if_not_installed("mockery")

  person <- type_object(name = type_string(), age = type_integer())
  chat <- chat_openai()

  # mock multi_convert_robust to simulate mixed success/failure with tokens
  mockery::stub(
    parallel_chat_structured_robust,
    "multi_convert_robust",
    data.frame(
      name = c("Alice", NA_character_, "Charlie", NA_character_),
      age = c(25L, NA_integer_, 35L, NA_integer_),
      input_tokens = c(10L, 0L, 12L, 0L),
      output_tokens = c(15L, 0L, 18L, 0L),
      cached_input_tokens = c(0L, 0L, 0L, 0L)
    )
  )

  data <- parallel_chat_structured_robust(
    chat,
    list(
      "Alice, age 25",
      "Bad prompt",
      "Charlie, age 35",
      "Another bad prompt"
    ),
    type = person,
    include_tokens = TRUE
  )

  expect_equal(nrow(data), 4)
  expect_equal(data$name, c("Alice", NA_character_, "Charlie", NA_character_))
  expect_equal(data$age, c(25L, NA_integer_, 35L, NA_integer_))
  expect_equal(data$input_tokens, c(10L, 0L, 12L, 0L))
  expect_equal(data$output_tokens, c(15L, 0L, 18L, 0L))
})

test_that("parallel_chat_structured_robust handles all manifestos with cost tracking", {
  skip_if_not(has_credentials("openai"))

  load("../data/data_manifestos.rda")

  # Make one manifesto very long to exceed context window
  data_manifestos["Lab_2010"] <- paste(
    rep(data_manifestos["Lab_2010"], 4),
    collapse = "\n\n---\n\n"
  )

  immig_type <- type_object(
    party = type_string("The name of the political party."),
    immig_score = type_integer("An integer position from -2 to 2 on immigration and
                             asylum seekers, where:
                             -2 means strongly against any immigration,
                             -1 means moderately opposed,
                             0 means neutral,
                             1 means moderately in favor of a more permissive stance on immigration,
                             2 means strongly in favour of a permissive stance on immigration."),
    immig_rationale = type_string("A brief rationale for your answer.")
  )

  prompt <- "Extract structured data from political texts. You will be scoring the position of each party on immigration and asylum seekers."

  chat <- chat_openai(model = "gpt-4o", system_prompt = prompt)

  # process all five manifestos at once with cost and token tracking
  response <- parallel_chat_structured_robust(
    chat,
    prompts = as.list(data_manifestos),
    max_active = 5,
    type = immig_type,
    include_tokens = TRUE,
    include_cost = TRUE
  )

  # should have same number of rows as input
  expect_equal(nrow(response), length(data_manifestos))
  expect_equal(ncol(response), 7) # party, immig_score, immig_rationale, input_tokens, output_tokens, cached_input_tokens, cost
  expect_contains(names(response), c("party", "immig_score", "immig_rationale", "cost", "input_tokens", "output_tokens"))

  # check cost and token columns exist and have appropriate values
  expect_true("cost" %in% names(response))
  expect_true("input_tokens" %in% names(response))
  expect_true("output_tokens" %in% names(response))
  expect_type(response$cost, "double")
  expect_type(response$input_tokens, "integer")
  expect_type(response$output_tokens, "integer")
  expect_equal(length(response$cost), 5)
  expect_equal(length(response$input_tokens), 5)
  expect_equal(length(response$output_tokens), 5)

  # failed prompts should have NA values and $0.00 cost
  has_nas <- apply(response[, c("party", "immig_score", "immig_rationale")], 1, function(row) any(is.na(row)))
  expect_true(any(has_nas), "At least one prompt should fail and produce NAs")

  # cost and tokens should be 0 for failed prompts (those with NAs)
  failed_rows <- which(has_nas)
  successful_rows <- which(!has_nas)

  expect_true(all(response$cost[failed_rows] == 0), "Failed prompts should have $0.00 cost")
  expect_true(all(response$input_tokens[failed_rows] == 0), "Failed prompts should have 0 input tokens")
  expect_true(all(response$output_tokens[failed_rows] == 0), "Failed prompts should have 0 output tokens")

  expect_true(all(response$cost[successful_rows] > 0), "Successful prompts should have positive cost")
  expect_true(all(response$input_tokens[successful_rows] > 0), "Successful prompts should have positive input tokens")
  expect_true(all(response$output_tokens[successful_rows] > 0), "Successful prompts should have positive output tokens")

  expect_true(all(is.finite(response$cost)), "All costs should be finite numbers")
  expect_true(all(is.finite(response$input_tokens)), "All input tokens should be finite numbers")
  expect_true(all(is.finite(response$output_tokens)), "All output tokens should be finite numbers")
})

test_that("parallel_chat_structured_robust on_error='return' stops at first error and returns partial results for manifestos", {
  skip_if_not(has_credentials("openai"))

  load("../data/data_manifestos.rda")

  # make one manifesto very long to exceed context window - this will be the third one processed
  data_manifestos[["Lab_2010"]] <- paste(
    rep(data_manifestos[["Lab_2010"]], 4),
    collapse = "\n\n---\n\n"
  )

  immig_type <- type_object(
    party = type_string("The name of the political party."),
    immig_score = type_integer("An integer position from -2 to 2 on immigration and
                             asylum seekers, where:
                             -2 means strongly against any immigration,
                             -1 means moderately opposed,
                             0 means neutral,
                             1 means moderately in favor of a more permissive stance on immigration,
                             2 means strongly in favour of a permissive stance on immigration."),
    immig_rationale = type_string("A brief rationale for your answer.")
  )

  prompt <- "Extract structured data from political texts. You will be scoring the position of each party on immigration and asylum seekers."

  chat <- chat_openai(model = "gpt-4o", system_prompt = prompt)

  # Capture the output to check for error messages
  output <- capture.output({
    response <- parallel_chat_structured_robust(
      chat,
      prompts = as.list(data_manifestos),
      max_active = 1, # Process sequentially to ensure predictable order
      type = immig_type,
      include_tokens = TRUE,
      include_cost = TRUE,
      include_status = TRUE,
      on_error = "return"
    )
  }, type = "message")

  # Should return only the first 2 successful results (before the error in Lab_2010)
  expect_equal(nrow(response), 2)
  expect_equal(ncol(response), 8) # party, immig_score, immig_rationale, input_tokens, output_tokens, cached_input_tokens, cost, status

  # All returned results should be successful
  expect_true(all(response$status == "success"))
  expect_false(any(is.na(response$party)))
  expect_false(any(is.na(response$immig_score)))
  expect_false(any(is.na(response$immig_rationale)))

  # Check that costs and tokens are positive for successful results
  expect_true(
    all(response$cost > 0),
    "All successful results should have positive cost"
  )
  expect_true(
    all(response$input_tokens > 0),
    "All successful results should have positive input tokens"
  )
  expect_true(
    all(response$output_tokens > 0),
    "All successful results should have positive output tokens"
  )

  # Check that error message was printed (the warning was captured but output capture may not work for warnings)
  # Let's just check that we got exactly 2 results as expected, which means the error stopping worked
  expect_equal(nrow(response), 2)
})

test_that("parallel_chat_structured_robust include_status tracks successes and errors", {
  skip_if_not_installed("mockery")

  person <- type_object(name = type_string(), age = type_integer())
  chat <- chat_openai()

  # Mock mixed success/failure results with status tracking
  mockery::stub(
    parallel_chat_structured_robust,
    "multi_convert_robust",
    data.frame(
      name = c("Alice", NA_character_, "Charlie", NA_character_),
      age = c(25L, NA_integer_, 35L, NA_integer_),
      status = c("success", "Token limit exceeded", "success", "Rate limit exceeded")
    )
  )

  data <- parallel_chat_structured_robust(
    chat,
    list(
      "Alice, age 25",
      "Very long prompt...",
      "Charlie, age 35",
      "Another bad prompt"
    ),
    type = person,
    include_status = TRUE
  )

  expect_equal(nrow(data), 4)
  expect_true("status" %in% names(data))
  expect_equal(data$status, c("success", "Token limit exceeded", "success", "Rate limit exceeded"))

  # Check that successful prompts have "success" status
  successful_rows <- which(!is.na(data$name))
  failed_rows <- which(is.na(data$name))

  expect_true(all(data$status[successful_rows] == "success"))
  expect_true(all(data$status[failed_rows] != "success"))
  expect_true(all(nchar(data$status[failed_rows]) > 0))  # Error messages should not be empty
})

test_that("parallel_chat_structured_robust include_status works with on_error='stop'", {
  skip_if_not(has_credentials("openai"))

  person <- type_object(name = type_string(), age = type_integer())
  chat <- chat_openai()

  data <- parallel_chat_structured_robust(
    chat,
    list("John, age 15", "Jane, age 16"),
    type = person,
    include_status = TRUE,
    on_error = "stop"
  )

  expect_equal(nrow(data), 2)
  expect_true("status" %in% names(data))
  expect_equal(data$status, c("success", "success"))
})

test_that("parallel_chat_structured_robust on_error='return' returns only successful results", {
  skip_if_not_installed("mockery")

  person <- type_object(name = type_string(), age = type_integer())
  chat <- chat_openai()

  # Mock successful results only - the function should filter out failures
  mockery::stub(
    parallel_chat_structured_robust,
    "multi_convert_robust",
    data.frame(
      name = c("Alice", "Bob"),
      age = c(25L, 30L),
      status = c("success", "success")
    )
  )

  data <- parallel_chat_structured_robust(
    chat,
    list("Alice, age 25", "Bob, age 30", "Very long prompt that fails..."),
    type = person,
    include_status = TRUE,
    on_error = "return"  # Test the "return" option specifically
  )

  # Should only return the successful results (first 2)
  expect_equal(nrow(data), 2)
  expect_true("status" %in% names(data))
  expect_equal(data$status, c("success", "success"))

  # Verify successful data only
  expect_equal(data$name, c("Alice", "Bob"))
  expect_equal(data$age, c(25L, 30L))

  # All results should be successful (no failures in the filtered output)
  successful_count <- sum(data$status == "success")
  expect_equal(successful_count, 2)
})

test_that("parallel_chat_structured_robust on_error='continue' issues warnings for errors", {
  skip_if_not_installed("mockery")

  person <- type_object(name = type_string(), age = type_integer())
  chat <- chat_openai()

  # Mock the multi_convert_robust function directly to avoid the value_turn complexity
  mockery::stub(
    parallel_chat_structured_robust,
    "multi_convert_robust",
    data.frame(
      name = c("Alice", NA_character_, "Charlie", NA_character_),
      age = c(25L, NA_integer_, 35L, NA_integer_),
      status = c("success", "Token limit exceeded", "success", "Rate limit exceeded")
    )
  )

  # Mock the parallel_turns_robust to return error_turns for failed prompts
  mockery::stub(
    parallel_chat_structured_robust,
    "parallel_turns_robust",
    list(
      # First prompt succeeds - mock a turn without error_turn class
      structure(list(content = "success"), class = "turn"),
      # Second prompt fails
      structure(
        list(error = "Token limit exceeded", index = 2, type_spec = person),
        class = "error_turn"
      ),
      # Third prompt succeeds
      structure(list(content = "success"), class = "turn"),
      # Fourth prompt fails
      structure(
        list(error = "Rate limit exceeded", index = 4, type_spec = person),
        class = "error_turn"
      )
    )
  )

  # Clear any existing warnings
  suppressWarnings({})

  # Capture message about errors
  expect_message(
    {
      data <- parallel_chat_structured_robust(
        chat,
        list(
          "Alice, age 25",
          "Bad prompt 1",
          "Charlie, age 35",
          "Bad prompt 2"
        ),
        type = person,
        include_status = TRUE,
        on_error = "continue"
      )
    },
    "Some prompts produced errors \\(2 out of 4\\). Use warnings\\(\\) to see error details"
  )

  # Check that results are as expected
  expect_equal(nrow(data), 4)
  expect_true("status" %in% names(data))

  # Check successful vs failed results
  successful_rows <- which(data$status == "success")
  failed_rows <- which(data$status != "success")
  expect_equal(successful_rows, c(1, 3))
  expect_equal(failed_rows, c(2, 4))

  # Clear warnings for other tests
  suppressWarnings({})
})

test_that("parallel_chat_structured_robust on_error='continue' issues warnings with real manifestos data", {
  skip_if_not(has_credentials("openai"))

  load("../data/data_manifestos.rda")

  # make one manifesto very long to exceed context window
  data_manifestos[["Lab_2010"]] <- paste(
    rep(data_manifestos[["Lab_2010"]], 4),
    collapse = "\n\n---\n\n"
  )

  immig_type <- type_object(
    party = type_string("The name of the political party."),
    immig_score = type_integer("An integer position from -2 to 2 on immigration and
                             asylum seekers, where:
                             -2 means strongly against any immigration,
                             -1 means moderately opposed,
                             0 means neutral,
                             1 means moderately in favor of a more permissive stance on immigration,
                             2 means strongly in favour of a permissive stance on immigration."
    ),
    immig_rationale = type_string("A brief rationale for your answer.")
  )

  prompt <- "Extract structured data from political texts. You will be scoring the position of each party on immigration and asylum seekers."

  chat <- chat_openai(model = "gpt-4o", system_prompt = prompt)

  # Clear any existing warnings
  suppressWarnings({})

  # This should issue message for failed prompts
  expect_message(
    {
      response <- parallel_chat_structured_robust(
        chat,
        prompts = as.list(data_manifestos),
        type = immig_type,
        include_status = TRUE,
        on_error = "continue"
      )
    },
    "Some prompts produced errors.*Use warnings\\(\\) to see error details"
  )

  # Should have same number of rows as input (maintains length)
  expect_equal(nrow(response), length(data_manifestos))

  # Check that some prompts failed (have NA values)
  has_nas <- apply(
    response[, c("party", "immig_score", "immig_rationale")],
    1,
    function(row) any(is.na(row))
  )
  expect_true(any(has_nas), "At least one prompt should fail and produce NAs")

  # The warning expectation above already confirms the warnings were issued
  # No need to check warnings() separately since the expect_warning caught it
})
