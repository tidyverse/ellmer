# Submit multiple chats in one batch

`batch_chat()` and `batch_chat_structured()` currently only work with
[`chat_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_openai.md)
and
[`chat_anthropic()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md).
They use the [OpenAI](https://platform.openai.com/docs/guides/batch) and
[Anthropic](https://docs.claude.com/en/docs/build-with-claude/batch-processing)
batch APIs which allow you to submit multiple requests simultaneously.
The results can take up to 24 hours to complete, but in return you pay
50% less than usual (but note that ellmer doesn't include this discount
in its pricing metadata). If you want to get results back more quickly,
or you're working with a different provider, you may want to use
[`parallel_chat()`](https://ellmer.tidyverse.org/dev/reference/parallel_chat.md)
instead.

Since batched requests can take a long time to complete, `batch_chat()`
requires a file path that is used to store information about the batch
so you never lose any work. You can either set `wait = FALSE` or simply
interrupt the waiting process, then later, either call `batch_chat()` to
resume where you left off or call `batch_chat_completed()` to see if the
results are ready to retrieve. `batch_chat()` will store the chat
responses in this file, so you can either keep it around to cache the
results, or delete it to free up disk space.

This API is marked as experimental since I don't yet know how to handle
errors in the most helpful way. Fortunately they don't seem to be
common, but if you have ideas, please let me know!

## Usage

``` r
batch_chat(chat, prompts, path, wait = TRUE, ignore_hash = FALSE)

batch_chat_text(chat, prompts, path, wait = TRUE, ignore_hash = FALSE)

batch_chat_structured(
  chat,
  prompts,
  path,
  type,
  wait = TRUE,
  ignore_hash = FALSE,
  convert = TRUE,
  include_tokens = FALSE,
  include_cost = FALSE
)

batch_chat_completed(chat, prompts, path)
```

## Arguments

- chat:

  A chat object created by a `chat_` function, or a string passed to
  [`chat()`](https://ellmer.tidyverse.org/dev/reference/chat-any.md).

- prompts:

  A vector created by
  [`interpolate()`](https://ellmer.tidyverse.org/dev/reference/interpolate.md)
  or a list of character vectors.

- path:

  Path to file (with `.json` extension) to store state.

  The file records a hash of the provider, the prompts, and the existing
  chat turns. If you attempt to reuse the same file with any of these
  being different, you'll get an error.

- wait:

  If `TRUE`, will wait for batch to complete. If `FALSE`, it will return
  `NULL` if the batch is not complete, and you can retrieve the results
  later by re-running `batch_chat()` when `batch_chat_completed()` is
  `TRUE`.

- ignore_hash:

  If `TRUE`, will only warn rather than error when the hash doesn't
  match. You can use this if ellmer has changed the hash structure and
  you're confident that you're reusing the same inputs.

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

For `batch_chat()`, a list of
[Chat](https://ellmer.tidyverse.org/dev/reference/Chat.md) objects, one
for each prompt. For `batch_chat_test()`, a character vector of text
responses. For `batch_chat_structured()`, a single structured data
object with one element for each prompt. Typically, when `type` is an
object, this will will be a data frame with one row for each prompt, and
one column for each property.

For any of the aboves, will return `NULL` if `wait = FALSE` and the job
is not complete.

## Examples

``` r
if (FALSE) { # has_credentials("openai")
chat <- chat_openai(model = "gpt-4.1-nano")

# Chat ----------------------------------------------------------------------

prompts <- interpolate("What do people from {{state.name}} bring to a potluck dinner?")
if (FALSE) { # \dontrun{
chats <- batch_chat(chat, prompts, path = "potluck.json")
chats
} # }

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
if (FALSE) { # \dontrun{
data <- batch_chat_structured(
  chat = chat,
  prompts = prompts,
  path = "people-data.json",
  type = type_person
)
data
} # }
}
```
