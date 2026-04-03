# Chat with an AWS bedrock model

[AWS Bedrock](https://aws.amazon.com/bedrock/) provides a number of
language models, including those from Anthropic's
[Claude](https://aws.amazon.com/bedrock/claude/), using the Bedrock
[Converse
API](https://docs.aws.amazon.com/bedrock/latest/APIReference/API_runtime_Converse.html).

### Authentication

Authentication is handled through {paws.common}, so if authentication
does not work for you automatically, you'll need to follow the advice at
<https://www.paws-r-sdk.com/#credentials>. In particular, if your org
uses AWS SSO, you'll need to run `aws sso login` at the terminal.

### Prompt caching

Bedrock supports [prompt
caching](https://docs.aws.amazon.com/bedrock/latest/userguide/prompt-caching.html)
via cache checkpoints. When caching is enabled, ellmer places cache
checkpoints on the system prompt and the last turn, so that the
conversation history is cached across turns.

By default (`cache = "auto"`), caching is enabled for models known to
support it (Anthropic Claude and Amazon Nova) and disabled for all other
models. You can also set `cache` to `"5m"` or `"1h"` to force a specific
TTL, or `"none"` to disable caching entirely. Note that individual
models may have minimum input token thresholds before caching takes
effect.

Note that
[`token_usage()`](https://ellmer.tidyverse.org/dev/reference/token_usage.md)
does not currently reflect the cost of writing to the cache, which is
priced at a premium over regular input tokens. Cache read savings are
reported correctly.

## Usage

``` r
chat_aws_bedrock(
  system_prompt = NULL,
  base_url = NULL,
  model = NULL,
  profile = NULL,
  cache = c("auto", "5m", "1h", "none"),
  params = NULL,
  api_args = list(),
  api_headers = character(),
  echo = NULL
)

models_aws_bedrock(profile = NULL, base_url = NULL)
```

## Arguments

- system_prompt:

  A system prompt to set the behavior of the assistant.

- base_url:

  The base URL to the endpoint; the default is OpenAI's public API.

- model:

  The model to use for the chat (defaults to
  "anthropic.claude-sonnet-4-5-20250929-v1:0"). We regularly update the
  default, so we strongly recommend explicitly specifying a model for
  anything other than casual use. Use `models_models_aws_bedrock()` to
  see all options. .

  While ellmer provides a default model, there's no guarantee that
  you'll have access to it, so you'll need to specify a model that you
  can. If you're using [cross-region
  inference](https://aws.amazon.com/blogs/machine-learning/getting-started-with-cross-region-inference-in-amazon-bedrock/),
  you'll need to use the inference profile ID, e.g.
  `model="us.anthropic.claude-sonnet-4-5-20250929-v1:0"`.

- profile:

  AWS profile to use.

- cache:

  How long to cache inputs? The default, `"auto"`, enables caching with
  a 5-minute TTL for models known to support it (Anthropic Claude and
  Amazon Nova) and disables caching for all other models. Set to `"5m"`
  or `"1h"` to force caching on, or `"none"` to disable it.

  See details below.

- params:

  Common model parameters, usually created by
  [`params()`](https://ellmer.tidyverse.org/dev/reference/params.md).

- api_args:

  Named list of arbitrary extra arguments appended to the body of every
  chat API call. Some useful arguments include:

      api_args = list(
        inferenceConfig = list(
          maxTokens = 100,
          temperature = 0.7,
          topP = 0.9,
          topK = 20
        )
      )

- api_headers:

  Named character vector of arbitrary extra headers appended to every
  chat API call.

- echo:

  One of the following options:

  - `none`: don't emit any output (default when running in a function).

  - `output`: echo text and tool-calling output as it streams in
    (default when running at the console).

  - `all`: echo all input and output.

  Note this only affects the
  [`chat()`](https://ellmer.tidyverse.org/dev/reference/chat-any.md)
  method.

## Value

A [Chat](https://ellmer.tidyverse.org/dev/reference/Chat.md) object.

## See also

Other chatbots:
[`chat_anthropic()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md),
[`chat_azure_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_azure_openai.md),
[`chat_cloudflare()`](https://ellmer.tidyverse.org/dev/reference/chat_cloudflare.md),
[`chat_databricks()`](https://ellmer.tidyverse.org/dev/reference/chat_databricks.md),
[`chat_deepseek()`](https://ellmer.tidyverse.org/dev/reference/chat_deepseek.md),
[`chat_github()`](https://ellmer.tidyverse.org/dev/reference/chat_github.md),
[`chat_google_gemini()`](https://ellmer.tidyverse.org/dev/reference/chat_google_gemini.md),
[`chat_groq()`](https://ellmer.tidyverse.org/dev/reference/chat_groq.md),
[`chat_huggingface()`](https://ellmer.tidyverse.org/dev/reference/chat_huggingface.md),
[`chat_mistral()`](https://ellmer.tidyverse.org/dev/reference/chat_mistral.md),
[`chat_ollama()`](https://ellmer.tidyverse.org/dev/reference/chat_ollama.md),
[`chat_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_openai.md),
[`chat_openai_compatible()`](https://ellmer.tidyverse.org/dev/reference/chat_openai_compatible.md),
[`chat_openrouter()`](https://ellmer.tidyverse.org/dev/reference/chat_openrouter.md),
[`chat_perplexity()`](https://ellmer.tidyverse.org/dev/reference/chat_perplexity.md),
[`chat_portkey()`](https://ellmer.tidyverse.org/dev/reference/chat_portkey.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage
chat <- chat_aws_bedrock()
chat$chat("Tell me three jokes about statisticians")
} # }
```
