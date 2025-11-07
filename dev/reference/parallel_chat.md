# Submit multiple chats in parallel

If you have multiple prompts, you can submit them in parallel. This is
typically considerably faster than submitting them in sequence,
especially with Gemini and OpenAI.

If you're using
[`chat_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_openai.md)
or
[`chat_anthropic()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md)
and you're willing to wait longer, you might want to use
[`batch_chat()`](https://ellmer.tidyverse.org/dev/reference/batch_chat.md)
instead, as it comes with a 50% discount in return for taking up to 24
hours.

## Usage

``` r
parallel_chat(
  chat,
  prompts,
  max_active = 10,
  rpm = 500,
  on_error = c("return", "continue", "stop")
)

parallel_chat_text(
  chat,
  prompts,
  max_active = 10,
  rpm = 500,
  on_error = c("return", "continue", "stop")
)

parallel_chat_structured(
  chat,
  prompts,
  type,
  convert = TRUE,
  include_tokens = FALSE,
  include_cost = FALSE,
  max_active = 10,
  rpm = 500,
  on_error = c("return", "continue", "stop")
)
```

## Arguments

- chat:

  A chat object created by a `chat_` function, or a string passed to
  [`chat()`](https://ellmer.tidyverse.org/dev/reference/chat-any.md).

- prompts:

  A vector created by
  [`interpolate()`](https://ellmer.tidyverse.org/dev/reference/interpolate.md)
  or a list of character vectors.

- max_active:

  The maximum number of simultaneous requests to send.

  For
  [`chat_anthropic()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md),
  note that the number of active connections is limited primarily by the
  output tokens per minute limit (OTPM) which is estimated from the
  `max_tokens` parameter, which defaults to 4096. That means if your
  usage tier limits you to 16,000 OTPM, you should either set
  `max_active = 4` (16,000 / 4096) to decrease the number of active
  connections or use
  [`params()`](https://ellmer.tidyverse.org/dev/reference/params.md) in
  [`chat_anthropic()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md)
  to decrease `max_tokens`.

- rpm:

  Maximum number of requests per minute.

- on_error:

  What to do when a request fails. One of:

  - `"return"` (the default): stop processing new requests, wait for in
    flight requests to finish, then return.

  - `"continue"`: keep going, performing every request.

  - `"stop"`: stop processing and throw an error.

- type:

  A type specification for the extracted data. Should be created with a
  [`type_()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md)
  function.

- convert:

  If `TRUE`, automatically convert from JSON lists to R data types using
  the schema. This typically works best when `type` is
  [`type_object()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md)
  as this will give you a data frame with one column for each property.
  If `FALSE`, returns a list.

- include_tokens:

  If `TRUE`, and the result is a data frame, will add `input_tokens` and
  `output_tokens` columns giving the total input and output tokens for
  each prompt.

- include_cost:

  If `TRUE`, and the result is a data frame, will add `cost` column
  giving the cost of each prompt.

## Value

For `parallel_chat()`, a list with one element for each prompt. Each
element is either a
[Chat](https://ellmer.tidyverse.org/dev/reference/Chat.md) object (if
successful), a `NULL` (if the request wasn't performed) or an error
object (if it failed).

For `parallel_chat_text()`, a character vector with one element for each
prompt. Requests that weren't succesful get an `NA`.

For `parallel_chat_structured()`, a single structured data object with
one element for each prompt. Typically, when `type` is an object, this
will be a tibble with one row for each prompt, and one column for each
property. If the output is a data frame, and some requests error, an
`.error` column will be added with the error objects.

## Examples

``` r
chat <- chat_openai()
#> Using model = "gpt-4.1".

# Chat ----------------------------------------------------------------------
country <- c("Canada", "New Zealand", "Jamaica", "United States")
prompts <- interpolate("What's the capital of {{country}}?")
parallel_chat(chat, prompts)
#> [[1]]
#> <Chat OpenAI/gpt-4.1 turns=2 input=13 output=11 cost=$0.00>
#> ── user ───────────────────────────────────────────────────────────────
#> What's the capital of Canada?
#> ── assistant [input=13 output=11 cost=$0.00] ──────────────────────────
#> The capital of Canada is **Ottawa**.
#> 
#> [[2]]
#> <Chat OpenAI/gpt-4.1 turns=2 input=14 output=12 cost=$0.00>
#> ── user ───────────────────────────────────────────────────────────────
#> What's the capital of New Zealand?
#> ── assistant [input=14 output=12 cost=$0.00] ──────────────────────────
#> The capital of New Zealand is **Wellington**.
#> 
#> [[3]]
#> <Chat OpenAI/gpt-4.1 turns=2 input=13 output=15 cost=$0.00>
#> ── user ───────────────────────────────────────────────────────────────
#> What's the capital of Jamaica?
#> ── assistant [input=13 output=15 cost=$0.00] ──────────────────────────
#> The capital of **Jamaica** is **Kingston**.
#> 
#> [[4]]
#> <Chat OpenAI/gpt-4.1 turns=2 input=14 output=15 cost=$0.00>
#> ── user ───────────────────────────────────────────────────────────────
#> What's the capital of United States?
#> ── assistant [input=14 output=15 cost=$0.00] ──────────────────────────
#> The capital of the United States is **Washington, D.C.**
#> 

# Structured data -----------------------------------------------------------
prompts <- list(
  "I go by Alex. 42 years on this planet and counting.",
  "Pleased to meet you! I'm Jamal, age 27.",
  "They call me Li Wei. Nineteen years young.",
  "Fatima here. Just celebrated my 35th birthday last week.",
  "The name's Robert - 51 years old and proud of it.",
  "Kwame here - just hit the big 5-0 this year."
)
type_person <- type_object(name = type_string(), age = type_number())
parallel_chat_structured(chat, prompts, type_person)
#> # A tibble: 6 × 2
#>   name     age
#>   <chr>  <dbl>
#> 1 Alex      42
#> 2 Jamal     27
#> 3 Li Wei    19
#> 4 Fatima    35
#> 5 Robert    51
#> 6 Kwame     50
```
