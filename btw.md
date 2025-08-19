---
client:
  provider: aws_bedrock
  model: us.anthropic.claude-sonnet-4-20250514-v1:0
---

## Chat functions with echo only (no params)

- [x] **chat_aws_bedrock**
  - [AWS Bedrock Converse API InferenceConfiguration](https://docs.aws.amazon.com/bedrock/latest/APIReference/API_runtime_InferenceConfiguration.html)
  - **Supported Parameters:**
    - `temperature` - Controls randomness in response generation (0.0 to 1.0)
    - `top_p` - Nucleus sampling parameter (0.0 to 1.0)
    - `max_tokens` - Maximum number of tokens to generate
    - `stop_sequences` - List of stop sequences (up to 4 items)

- [ ] **chat_databricks**
  - [Databricks Foundation Model APIs Chat Request](https://docs.databricks.com/aws/en/machine-learning/foundation-model-apis/api-reference#chat-request)
  - **Supported Parameters:**
    - `temperature` - Sampling temperature for randomness (0.0 to 2.0)
    - `top_p` - Nucleus sampling threshold (0.0 to 1.0)
    - `top_k` - Top-k filtering for most likely tokens
    - `max_tokens` - Maximum number of tokens to generate
    - `stop` - Stop sequences (string or list of strings)

- [ ] **chat_deepseek**
  - [Chat Completions API](https://platform.deepseek.com/api-docs/api/create-chat-completion)
  - **Supported Parameters (OpenAI-compatible):**
    - `temperature` - Controls randomness (0.0 to 2.0)
    - `top_p` - Nucleus sampling parameter (0.0 to 1.0)
    - `max_tokens` - Maximum tokens to generate
    - `stop` - Stop sequences
    - `frequency_penalty` - Frequency penalty for repetition
    - `presence_penalty` - Presence penalty for new topics

- [ ] **chat_github**
  - Since GitHub Models uses OpenAI-compatible APIs but the specific parameter documentation wasn't fully visible in the API reference tables, GitHub Models likely supports OpenAI-standard parameters. However, based on the quickstart examples showing `modelParameters` with `temperature`, it appears to support at least basic parameters.
  - **Supported Parameters (likely):**
    - `temperature` - Controls randomness in response generation
    - Additional parameters would need verification from the full API specification
  - [GitHub Models Quickstart](https://docs.github.com/en/github-models/quickstart)

- [ ] **chat_groq**
  - [Groq Chat Completions API Reference](https://console.groq.com/docs/api-reference#chat-create)
  - **Supported Parameters:**
    - `temperature` - Sampling temperature between 0 and 2 (default: 1)
    - `top_p` - Nucleus sampling parameter between 0 and 1 (default: 1)
    - `max_completion_tokens` - Maximum number of tokens to generate
    - `seed` - Seed for deterministic sampling
    - `frequency_penalty` - Frequency penalty for generated tokens (range: -2 to 2, default: 0)
    - `presence_penalty` - Presence penalty for generated tokens (range: -2 to 2, default: 0)
    - `stop` - Stop sequences (up to 4 sequences)

- [ ] **chat_ollama**
  - [Ollama Chat API Documentation](https://github.com/ollama/ollama/blob/main/docs/api.md#generate-a-chat-completion)
  - **Supported Parameters (via options object):**
    - `temperature` - Sampling temperature (shown in examples)
    - `top_p` - Nucleus sampling parameter (shown in examples)
    - `top_k` - Top-k sampling parameter (shown in examples)
    - `seed` - Seed for reproducible outputs (shown in examples)
    - `frequency_penalty` - Frequency penalty for repetition (shown in examples)
    - `presence_penalty` - Presence penalty for new topics (shown in examples)
    - `stop` - Stop sequences (shown in examples)
    - `num_predict` - Maximum number of tokens to generate (equivalent to max_tokens)

- [ ] **chat_openrouter**
  - [OpenRouter API Parameters](https://openrouter.ai/docs/api-reference/parameters)
  - **Supported Parameters:**
    - `temperature` - Controls randomness in response generation (0.0 to 2.0)
    - `top_p` - Nucleus sampling parameter for token selection (0.0 to 1.0)
    - `top_k` - Limits model's choice of tokens at each step (0 or above)
    - `max_tokens` - Maximum number of tokens to generate (1 or above)
    - `stop` - Stop generation sequences (array)
    - `frequency_penalty` - Reduces repetition based on token frequency (-2.0 to 2.0)
    - `presence_penalty` - Reduces repetition of tokens from input (-2.0 to 2.0)
    - `seed` - Deterministic sampling seed (integer)
    - `logprobs` - Return log probabilities of output tokens (boolean)

- [ ] **chat_perplexity**
  - [Perplexity Chat Completions API](https://docs.perplexity.ai/api-reference/chat-completions-post)
  - **Supported Parameters:**
    - `temperature` - Controls randomness in response generation (0 to 2, default: 0.2)
    - `top_p` - Nucleus sampling threshold (0 to 1, default: 0.9)
    - `top_k` - Top-k filtering for most likely tokens (number, default: 0)
    - `max_tokens` - Maximum number of completion tokens (integer)
    - `frequency_penalty` - Decreases likelihood of repetition based on frequency (0 to 2.0, default: 0)
    - `presence_penalty` - Increases likelihood of discussing new topics (0 to 2.0, default: 0)

- [ ] **chat_vllm**
  - [vLLM OpenAI Compatible Server](https://docs.vllm.ai/en/latest/serving/openai_compatible_server.html)
  - **Supported Parameters (OpenAI-compatible):**
    - `temperature` - Controls randomness in response generation
    - `top_p` - Nucleus sampling parameter
    - `top_k` - Top-k sampling parameter (vLLM-specific, passed via extra_body)
    - `max_tokens` - Maximum number of tokens to generate
    - `stop` - Stop sequences
    - `frequency_penalty` - Frequency penalty for repetition
    - `presence_penalty` - Presence penalty for new topics
    - `seed` - Seed for deterministic sampling
    - `logprobs` - Return log probabilities

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
Implement a `chat_params()` method that maps standardized parameter names to provider-specific names:

```r
method(chat_params, ProviderExample) <- function(provider, params) {
  standardise_params(
    params,
    c(
      # Map ellmer standard names to provider API names
      temperature = "temperature",
      top_p = "top_p",
      max_tokens = "max_tokens",  # or maxTokens, max_completion_tokens, etc.
      stop_sequences = "stop",    # or stopSequences, stop_sequences, etc.
      # ... other supported parameters
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
