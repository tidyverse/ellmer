# ellmer

ellmer makes it easy to use large language models (LLM) from R. It
supports a wide variety of LLM providers and implements a rich set of
features including streaming outputs, tool/function calling, structured
data extraction, and more.

ellmer is one of a number of LLM-related packages created by Posit:

- Looking for something similar in python? Check out
  [chatlas](https://github.com/posit-dev/chatlas)!
- Want to evaluate your LLMs? Try
  [vitals](https://vitals.tidyverse.org).
- Need RAG? Take a look at [ragnar](https://ragnar.tidyverse.org).
- Want to make a beautiful LLM powered chatbot? Consider
  [shinychat](https://posit-dev.github.io/shinychat/).
- Working with MCP? Check out
  [mcptools](https://posit-dev.github.io/mcptools/).

## Installation

You can install ellmer from CRAN with:

``` r
install.packages("ellmer")
```

## Providers

ellmer supports a wide variety of model providers:

- Anthropic’s Claude:
  [`chat_anthropic()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md).
- AWS Bedrock:
  [`chat_aws_bedrock()`](https://ellmer.tidyverse.org/dev/reference/chat_aws_bedrock.md).
- Azure OpenAI:
  [`chat_azure_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_azure_openai.md).
- Cloudflare:
  [`chat_cloudflare()`](https://ellmer.tidyverse.org/dev/reference/chat_cloudflare.md).
- Databricks:
  [`chat_databricks()`](https://ellmer.tidyverse.org/dev/reference/chat_databricks.md).
- DeepSeek:
  [`chat_deepseek()`](https://ellmer.tidyverse.org/dev/reference/chat_deepseek.md).
- GitHub model marketplace:
  [`chat_github()`](https://ellmer.tidyverse.org/dev/reference/chat_github.md).
- Google Gemini/Vertex AI:
  [`chat_google_gemini()`](https://ellmer.tidyverse.org/dev/reference/chat_google_gemini.md),
  [`chat_google_vertex()`](https://ellmer.tidyverse.org/dev/reference/chat_google_gemini.md).
- Groq:
  [`chat_groq()`](https://ellmer.tidyverse.org/dev/reference/chat_groq.md).
- Hugging Face:
  [`chat_huggingface()`](https://ellmer.tidyverse.org/dev/reference/chat_huggingface.md).
- Mistral:
  [`chat_mistral()`](https://ellmer.tidyverse.org/dev/reference/chat_mistral.md).
- Ollama:
  [`chat_ollama()`](https://ellmer.tidyverse.org/dev/reference/chat_ollama.md).
- OpenAI:
  [`chat_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_openai.md).
- OpenRouter:
  [`chat_openrouter()`](https://ellmer.tidyverse.org/dev/reference/chat_openrouter.md).
- perplexity.ai:
  [`chat_perplexity()`](https://ellmer.tidyverse.org/dev/reference/chat_perplexity.md).
- Snowflake Cortex:
  [`chat_snowflake()`](https://ellmer.tidyverse.org/dev/reference/chat_snowflake.md)
  and `chat_cortex_analyst()`.
- VLLM:
  [`chat_vllm()`](https://ellmer.tidyverse.org/dev/reference/chat_vllm.md).

### Provider/model choice

If you’re using ellmer inside an organisation, you may have internal
policies that limit you to models from big cloud providers,
e.g. [`chat_azure_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_azure_openai.md),
[`chat_aws_bedrock()`](https://ellmer.tidyverse.org/dev/reference/chat_aws_bedrock.md),
[`chat_databricks()`](https://ellmer.tidyverse.org/dev/reference/chat_databricks.md),
or
[`chat_snowflake()`](https://ellmer.tidyverse.org/dev/reference/chat_snowflake.md).

If you’re using ellmer for your own exploration, you’ll have a lot more
freedom, so we have a few recommendations to help you get started:

- [`chat_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_openai.md)
  or
  [`chat_anthropic()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md)
  are good places to start.
  [`chat_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_openai.md)
  defaults to **GPT-4.1**, but you can use `model = "gpt-4-1-nano"` for
  a cheaper, faster model, or `model = "o3"` for more complex reasoning.
  [`chat_anthropic()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md)
  is also good; it defaults to **Claude 4.0 Sonnet**, which we have
  found to be particularly good at writing R code.

- [`chat_google_gemini()`](https://ellmer.tidyverse.org/dev/reference/chat_google_gemini.md)
  is a strong model with generous free tier (with the downside that
  [your data is
  used](https://ai.google.dev/gemini-api/terms#unpaid-services) to
  improve the model), making it a great place to start if you don’t want
  to spend any money.

- [`chat_ollama()`](https://ellmer.tidyverse.org/dev/reference/chat_ollama.md),
  which uses [Ollama](https://ollama.com), allows you to run models on
  your own computer. While the biggest models you can run locally aren’t
  as good as the state of the art hosted models, they don’t share your
  data and are effectively free.

### Authentication

Authentication works a little differently depending on the provider. A
few popular ones (including OpenAI and Anthropic) require you to obtain
an API key. We recommend you save it in an environment variable rather
than using it directly in your code, and if you deploy an app or report
that uses ellmer to another system, you’ll need to ensure that this
environment variable is available there, too.

ellmer also automatically detects many of the OAuth or IAM-based
credentials used by the big cloud providers (currently
[`chat_azure_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_azure_openai.md),
[`chat_aws_bedrock()`](https://ellmer.tidyverse.org/dev/reference/chat_aws_bedrock.md),
[`chat_databricks()`](https://ellmer.tidyverse.org/dev/reference/chat_databricks.md),
[`chat_snowflake()`](https://ellmer.tidyverse.org/dev/reference/chat_snowflake.md)).
That includes credentials for these platforms managed by [Posit
Workbench](https://docs.posit.co/ide/server-pro/user/posit-workbench/managed-credentials/managed-credentials.html)
and [Posit
Connect](https://docs.posit.co/connect/user/oauth-integrations/#adding-oauth-integrations-to-deployed-content).

If you find cases where ellmer cannot detect credentials from one of
these cloud providers, feel free to open an issue; we’re happy to add
more auth mechanisms if needed.

## Using ellmer

You can work with ellmer in several different ways, depending on whether
you are working interactively or programmatically. They all start with
creating a new chat object:

``` r
library(ellmer)

chat <- chat_openai("Be terse", model = "gpt-4o-mini")
```

Chat objects are stateful [R6 objects](https://r6.r-lib.org): they
retain the context of the conversation, so each new query builds on the
previous ones. You call their methods with `$`.

### Interactive chat console

The most interactive and least programmatic way of using ellmer is to
chat directly in your R console or browser with `live_console(chat)` or
[`live_browser()`](https://ellmer.tidyverse.org/dev/reference/live_console.md):

``` r
live_console(chat)
#> ╔════════════════════════════════════════════════════════╗
#> ║  Entering chat console. Use """ for multi-line input.  ║
#> ║  Press Ctrl+C to quit.                                 ║
#> ╚════════════════════════════════════════════════════════╝
#> >>> Who were the original creators of R?
#> R was originally created by Ross Ihaka and Robert Gentleman at the University of
#> Auckland, New Zealand.
#>
#> >>> When was that?
#> R was initially released in 1995. Development began a few years prior to that,
#> in the early 1990s.
```

Keep in mind that the chat object retains state, so when you enter the
chat console, any previous interactions with that chat object are still
part of the conversation, and any interactions you have in the chat
console will persist after you exit back to the R prompt. This is true
regardless of which chat function you use.

### Interactive method call

The second most interactive way to chat is to call the
[`chat()`](https://ellmer.tidyverse.org/dev/reference/chat-any.md)
method:

``` r
chat$chat("What preceding languages most influenced R?")
#> R was primarily influenced by S, a language developed at Bell Laboratories. 
#> Other notable influences include:
#> 
#> 1. **Scheme** - For functional programming concepts.
#> 2. **LISP** - For its powerful data manipulation features.
#> 3. **C** - For performance and system-level access.
#> 4. **Fortran** - For numerical and statistical computations.
#> 
#> These languages contributed to R's syntax, data structures, and functional 
#> programming capabilities.
```

If you initialize the chat object in the global environment, the `chat`
method will stream the response to the console. When the entire response
is received, it’s also (invisibly) returned as a character vector. This
is useful when you want to see the response as it arrives, but you don’t
want to enter the chat console.

If you want to ask a question about an image, you can pass one or more
additional input arguments using
[`content_image_file()`](https://ellmer.tidyverse.org/dev/reference/content_image_url.md)
and/or
[`content_image_url()`](https://ellmer.tidyverse.org/dev/reference/content_image_url.md):

``` r
chat$chat(
  content_image_url("https://www.r-project.org/Rlogo.png"),
  "Can you explain this logo?"
)
#> The logo consists of a stylized letter "R" in blue, surrounded by a gray oval 
#> shape. The design reflects the programming language R, which is widely used for
#> statistical computing and graphics. The color choice often symbolizes clarity 
#> and professionalism, aligning with R's use in data analysis and research. The 
#> logo encapsulates the language's focus on data visualization and statistical 
#> methods.
```

### Streaming vs capturing

In most circumstances, ellmer will stream the output to the console. You
can take control of this by setting the `echo` argument either when
creating the chat object or when calling `$chat()`. Set `echo = "none"`
to return a string instead:

``` r
my_function <- function() {
  chat <- chat_openai("Be terse", model = "gpt-4o-mini", echo = "none")
  chat$chat("What is 6 times 7?")
}
str(my_function())
#>  'ellmer_output' chr "6 times 7 is 42."
```

If needed, you can manually control this behaviour with the `echo`
argument. This is useful for programming with ellmer when the result is
either not intended for human consumption or when you want to process
the response before displaying it.

## Learning more

ellmer comes with a bunch of vignettes to help you learn more:

- Learn key vocabulary and see example use cases in
  [`vignette("ellmer")`](https://ellmer.tidyverse.org/dev/articles/ellmer.md).
- Learn how to design your prompt in
  [`vignette("prompt-design")`](https://ellmer.tidyverse.org/dev/articles/prompt-design.md).
- Learn about tool/function calling in
  [`vignette("tool-calling")`](https://ellmer.tidyverse.org/dev/articles/tool-calling.md).
- Learn how to extract structured data in
  [`vignette("structured-data")`](https://ellmer.tidyverse.org/dev/articles/structured-data.md).
- Learn about streaming and async APIs in
  [`vignette("streaming-async")`](https://ellmer.tidyverse.org/dev/articles/streaming-async.md).
