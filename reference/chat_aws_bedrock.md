# Chat with an AWS bedrock model

![\[Official supported provider\]](figures/support-official.svg)

[AWS Bedrock](https://aws.amazon.com/bedrock/) provides a number of
language models, including those from Anthropic's
[Claude](https://aws.amazon.com/bedrock/claude/). Most are served
through the Bedrock [Converse
API](https://docs.aws.amazon.com/bedrock/latest/APIReference/API_runtime_Converse.html),
with some only available through the Anthropic Messages or OpenAI
Responses APIs; see the `api` argument for details.

### APIs and endpoints

Bedrock serves models from two endpoints, and `api` selects which one to
use and which request format to send:

- `"converse"` uses the Converse API on the `bedrock-runtime` endpoint.
  This reaches the great majority of Bedrock models, and is what ellmer
  has always used.

- `"messages"` uses the Anthropic Messages API on the `bedrock-mantle`
  endpoint. Only Claude models are available here, but it includes some
  (like Claude Mythos) that Converse does not serve at all.

- `"responses"` uses the OpenAI Responses API on the `bedrock-mantle`
  endpoint. This reaches OpenAI and xAI models that Converse can't
  serve, typically newer models that Converse hasn't picked up yet. Note
  that mantle serves newer models from `/openai/v1` and older
  open-weight models like gpt-oss from `/v1`; ellmer uses the former, so
  reaching the latter needs an explicit `base_url`. They're all
  available through `"converse"` anyway.

By default ellmer picks the API from `model`, using `"converse"`
whenever it can serve the model and for any model ellmer doesn't
recognize. Set `api` explicitly to override this.

The set of models that need mantle shrinks over time as AWS adds them to
Converse, so a model that needs `"responses"` today may route to
`"converse"` in a later ellmer release. Note also that Converse usually
needs an inference profile ID (`"us.openai.gpt-5.6-sol"`) while mantle
wants the bare model ID (`"openai.gpt-5.6-sol"`); ellmer strips the
prefix for mantle, so the inference profile ID works with either and is
the safer choice.

Note that the two endpoints have separate token quotas, so moving a
model from one to the other changes which quota it consumes.

### Authentication

`chat_aws_bedrock()` uses {paws.common} to resolve credentials, trying
the following strategies in order:

- A bearer token set in the `AWS_BEARER_TOKEN_BEDROCK` or
  `AWS_BEARER_TOKEN` environment variable. This is used by enterprise
  API gateways that issue API keys instead of IAM credentials. See the
  [AWS
  documentation](https://docs.aws.amazon.com/bedrock/latest/userguide/api-keys-use.html)
  for details.

- Standard IAM credentials resolved from environment variables, AWS
  config files, SSO, or instance metadata. See
  <https://www.paws-r-sdk.com/#credentials> for details. If your org
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
[`token_usage()`](https://ellmer.tidyverse.org/reference/token_usage.md)
does not currently reflect the cost of writing to the cache, which is
priced at a premium over regular input tokens. Cache read savings are
reported correctly.

## Usage

``` r
chat_aws_bedrock(
  system_prompt = NULL,
  base_url = NULL,
  model = NULL,
  api = NULL,
  profile = NULL,
  cache = c("auto", "5m", "1h", "none"),
  params = NULL,
  api_args = list(),
  api_headers = character(),
  echo = NULL
)

models_aws_bedrock(profile = NULL, base_url = NULL, api = NULL)
```

## Arguments

- system_prompt:

  A system prompt to set the behavior of the assistant.

- base_url:

  The base URL to the endpoint; the default is the standard endpoint for
  the selected `api` and your region, matching the official SDKs'
  endpoint override environment variables:
  `AWS_ENDPOINT_URL_BEDROCK_RUNTIME` for `"converse"`, and
  `AWS_ENDPOINT_URL_BEDROCK_MANTLE` for `"messages"` and `"responses"`
  (which append their API-specific path to the override).
  `models_aws_bedrock()` talks to a different AWS service, so it honors
  `AWS_ENDPOINT_URL_BEDROCK` instead.

- model:

  The model to use for the chat (defaults to
  "us.anthropic.claude-sonnet-5"). We regularly update the default, so
  we strongly recommend explicitly specifying a model for anything other
  than casual use. Use `models_models_aws_bedrock()` to see all options.
  .

  While ellmer provides a default model, there's no guarantee that
  you'll have access to it, so you'll need to specify a model that you
  can. If you're using [cross-region
  inference](https://aws.amazon.com/blogs/machine-learning/getting-started-with-cross-region-inference-in-amazon-bedrock/),
  you'll need to use the inference profile ID, e.g.
  `model="us.anthropic.claude-sonnet-5"`.

- api:

  Which Bedrock API to use: `"converse"`, `"messages"`, or
  `"responses"`. The default, `NULL`, picks the API from `model`,
  falling back to `"converse"` for unrecognized models.

  See details below.

- profile:

  AWS profile to use.

- cache:

  How long to cache inputs? The default, `"auto"`, enables caching with
  a 5-minute TTL for models known to support it (Anthropic Claude and
  Amazon Nova) and disables caching for all other models. Set to `"5m"`
  or `"1h"` to force caching on, or `"none"` to disable it.

  Not supported when `api = "responses"`, which caches automatically.

  See details below.

- params:

  Common model parameters, usually created by
  [`params()`](https://ellmer.tidyverse.org/reference/params.md).

- api_args:

  Named list of arbitrary extra arguments appended to the body of every
  chat API call. Use `params` for common parameters. Model-specific
  inference parameters can be provided using the
  `additionalModelRequestFields` field (`api = "converse"` only), for
  example to enable thinking effort in Anthropic Claude models:

      api_args = list(
        additionalModelRequestFields = list(
          thinking = list(type = "enabled", budget_tokens = 4000)
        )
      )

  See
  <https://docs.aws.amazon.com/bedrock/latest/userguide/conversation-inference-call.html>
  for more details.

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
  [`chat()`](https://ellmer.tidyverse.org/reference/chat-any.md) method.

## Value

A [Chat](https://ellmer.tidyverse.org/reference/Chat.md) object.

## See also

Other chatbots:
[`chat_anthropic()`](https://ellmer.tidyverse.org/reference/chat_anthropic.md),
[`chat_azure_openai()`](https://ellmer.tidyverse.org/reference/chat_azure_openai.md),
[`chat_cloudflare()`](https://ellmer.tidyverse.org/reference/chat_cloudflare.md),
[`chat_databricks()`](https://ellmer.tidyverse.org/reference/chat_databricks.md),
[`chat_deepseek()`](https://ellmer.tidyverse.org/reference/chat_deepseek.md),
[`chat_github()`](https://ellmer.tidyverse.org/reference/chat_github.md),
[`chat_google_gemini()`](https://ellmer.tidyverse.org/reference/chat_google_gemini.md),
[`chat_groq()`](https://ellmer.tidyverse.org/reference/chat_groq.md),
[`chat_huggingface()`](https://ellmer.tidyverse.org/reference/chat_huggingface.md),
[`chat_lmstudio()`](https://ellmer.tidyverse.org/reference/chat_lmstudio.md),
[`chat_mistral()`](https://ellmer.tidyverse.org/reference/chat_mistral.md),
[`chat_ollama()`](https://ellmer.tidyverse.org/reference/chat_ollama.md),
[`chat_openai()`](https://ellmer.tidyverse.org/reference/chat_openai.md),
[`chat_openai_compatible()`](https://ellmer.tidyverse.org/reference/chat_openai_compatible.md),
[`chat_openrouter()`](https://ellmer.tidyverse.org/reference/chat_openrouter.md),
[`chat_perplexity()`](https://ellmer.tidyverse.org/reference/chat_perplexity.md),
[`chat_portkey()`](https://ellmer.tidyverse.org/reference/chat_portkey.md),
[`chat_posit()`](https://ellmer.tidyverse.org/reference/chat_posit.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage
chat <- chat_aws_bedrock()
chat$chat("Tell me three jokes about statisticians")
} # }
```
