---
client:
  provider: aws_bedrock
  model: us.anthropic.claude-sonnet-4-20250514-v1:0
---

## About `ellmer::params()`

This helper function makes it easier to create a list of parameters used
across many models. The parameter names are automatically standardised and
included in the correctly place in the API call.

Note that parameters that are not supported by a given provider will generate
a warning, not an error. This allows you to use the same set of parameters
across multiple providers.

```r
#' @param temperature Temperature of the sampling distribution.
#' @param top_p The cumulative probability for token selection.
#' @param top_k The number of highest probability vocabulary tokens to keep.
#' @param frequency_penalty Frequency penalty for generated tokens.
#' @param presence_penalty Presence penalty for generated tokens.
#' @param seed Seed for random number generator.
#' @param max_tokens Maximum number of tokens to generate.
#' @param log_probs Include the log probabilities in the output?
#' @param stop_sequences A character vector of tokens to stop generation on.
```

## Chat functions with echo only (no params)

Supported parameters are listed as `<provider name> = <standard name>`.

Here’s the list with each mapping rewritten to swap the order (right-hand side first, equals, then left-hand side). Example applied: stop = stop_sequences.

- [x] **chat_aws_bedrock**
  - [AWS Bedrock Converse API InferenceConfiguration](https://docs.aws.amazon.com/bedrock/latest/APIReference/API_runtime_InferenceConfiguration.html)
  - Supported parameters:
    - `temperature = temperature` → `temperature = temperature` (unchanged)
    - `topP = top_p`
    - `maxTokens = max_tokens`
    - `stopSequences = stop_sequences`

- [x] **chat_databricks**
  - [Databricks Foundation Model APIs Chat Request](https://docs.databricks.com/aws/en/machine-learning/foundation-model-apis/api-reference#chat-request)
  - Supported parameters:
    - `temperature = temperature`
    - `top_p = top_p`
    - `top_k = top_k`
    - `max_tokens = max_tokens`
    - `stop = stop_sequences`

- [x] **chat_deepseek**
  - [Chat Completions API](https://api-docs.deepseek.com/api/create-chat-completion)
  - Supported parameters:
    - `temperature = temperature`
    - `top_p = top_p`
    - `max_tokens = max_tokens`
    - `frequency_penalty = frequency_penalty`
    - `presence_penalty = presence_penalty`
    - `logprobs = log_probs`
    - `top_logprobs = top_k`
    - `stop = stop_sequences`

- [x] **chat_github**
  - [REST API endpoints for models inference](https://docs.github.com/en/rest/models/inference?apiVersion=2022-11-28)
  - Supported parameters:
    - `frequency_penalty = frequency_penalty`
    - `max_tokens = max_tokens`
    - `presence_penalty = presence_penalty`
    - `seed = seed`
    - `stop = stop_sequences`
    - `temperature = temperature`
    - `top_p = top_p`

- [x] **chat_groq**
  - [Groq Chat Completions API Reference](https://console.groq.com/docs/api-reference#chat-create)
  - Supported parameters:
    - `frequency_penalty = frequency_penalty`
    - `logprobs = log_probs`
    - `max_completion_tokens = max_tokens`
    - `presence_penalty = presence_penalty`
    - `seed = seed`
    - `stop = stop_sequences`
    - `temperature = temperature`
    - `top_logprobs = top_k`
    - `top_p = top_p`

- [x] **chat_ollama**
  - [Ollama Chat API Documentation](https://github.com/ollama/ollama/blob/main/docs/api.md#generate-a-chat-completion)
  - Supported parameters
    - `frequency_penalty = frequency_penalty`
    - `presence_penalty = presence_penalty`
    - `seed = seed`
    - `stop = stop_sequences`
    - `temperature = temperature`
    - `top_p = top_p`
    - `max_tokens = max_tokens`

- [x] **chat_openrouter**
  - [OpenRouter API Parameters](https://openrouter.ai/docs/api-reference/parameters)
  - Supported parameters:
    - `temperature = temperature`
    - `top_p = top_p`
    - `top_k = top_k`
    - `frequency_penalty = frequency_penalty`
    - `presence_penalty = presence_penalty`
    - `seed = seed`
    - `max_tokens = max_tokens`
    - `logprobs = log_probs`
    - `stop = stop_sequences`

- [x] **chat_perplexity**
  - [Perplexity Chat Completions API](https://docs.perplexity.ai/api-reference/chat-completions-post)
  - Supported parameters:
    - `max_tokens = max_tokens`
    - `temperature = temperature`
    - `top_p = top_p`
    - `top_k = top_k`
    - `presence_penalty = presence_penalty`
    - `frequency_penalty = frequency_penalty`

- [x] **chat_vllm**
  - [vLLM OpenAI Compatible Server](https://docs.vllm.ai/en/latest/serving/openai_compatible_server.html)
  - OpenAI-compatible, use inherited OpenAI behavior

## How to implement `params` support

To add `params` support to chat functions that currently only support `echo`, follow these patterns:

### 1. Function signature changes
Add `params = NULL` parameter to the function signature:

```r
chat_example <- function(
  system_prompt = NULL,
  # ... other existing parameters ...
  params = NULL,  # <- ADD THIS
  echo = c("none", "output", "all")
) {
  # ...
}
```

### 2. Initialize params in function body
Set default params and store in provider:

```r
chat_example <- function(...) {
  # ... existing code ...

  params <- params %||% params()  # <- ADD THIS LINE

  provider <- ProviderExample(
    # ... existing properties ...
    params = params,  # <- ADD THIS PROPERTY
    # ... other properties ...
  )

  # ... rest of function ...
}
```

### 3. Add chat_params method
Implement a `chat_params()` method that maps standardized parameter names to provider-specific names. Include a link to the provider's documentation for reference.

```r
method(chat_params, ProviderExample) <- function(provider, params) {
  # Link to documentation for provider-specific parameters
  standardise_params(
    params,
    c(
      # Map provider API parameter names to ellmer standard names
      maxTokens = "max_tokens",
      {providerName} = "{standardName}",  # <- Replace with param names
      # ... only the parameters that the provider supports ...
    )
  )
}
```

### 4. Use params in request method
**First, examine the existing implementation** to understand:
- Which method handles request body construction (`chat_body()`, `chat_request()`, etc.)
- How the provider structures parameters in the API request
- Whether parameters go directly in the body or in a nested object

Then modify the appropriate method to include standardized params. Common patterns:

**For providers with direct parameter inclusion (like OpenAI):**
```r
method(chat_body, ProviderExample) <- function(...) {
  params <- chat_params(provider, provider@params)

  compact(list2(
    # ... existing body fields ...
    !!!params  # <- Spread params directly into body
  ))
}
```

**For providers with nested parameter objects (like AWS Bedrock):**
```r
method(chat_request, ProviderExample) <- function(...) {
  params <- chat_params(provider, provider@params)

  # Merge with existing config, giving precedence to manual api_args
  config <- modify_list(params, provider@extra_args$parameterConfig %||% list())

  body <- list(
    # ... other body fields ...
    parameterConfig = config
  )
  # ...
}
```

### Key points:
- **Always examine the existing implementation first** using the available tools
- **Use `params %||% params()`** to provide default empty params
- **Use `standardise_params()`** to map ellmer standard names to provider-specific API parameter names
- **Handle integration with existing `api_args`** appropriately for the provider's API structure
- **Parameter placement varies by provider** - some put params directly in request body, others use nested objects
