# Package index

## Chatbots

ellmer provides a simple interface to a wide range of LLM providers. Use
the `chat_` functions to initialize a `Chat` object for a specific
provider and model. Once created, use the methods of the `Chat` object
to send messages, receive responses, manage tools and extract structured
data.

- [`chat()`](https://ellmer.tidyverse.org/dev/reference/chat-any.md) :
  Chat with any provider
- [`chat_anthropic()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md)
  [`chat_claude()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md)
  [`models_claude()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md)
  [`models_anthropic()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md)
  : Chat with an Anthropic Claude model
- [`chat_aws_bedrock()`](https://ellmer.tidyverse.org/dev/reference/chat_aws_bedrock.md)
  [`models_aws_bedrock()`](https://ellmer.tidyverse.org/dev/reference/chat_aws_bedrock.md)
  : Chat with an AWS bedrock model
- [`chat_azure_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_azure_openai.md)
  : Chat with a model hosted on Azure OpenAI
- [`chat_cloudflare()`](https://ellmer.tidyverse.org/dev/reference/chat_cloudflare.md)
  : Chat with a model hosted on CloudFlare
- [`chat_databricks()`](https://ellmer.tidyverse.org/dev/reference/chat_databricks.md)
  : Chat with a model hosted on Databricks
- [`chat_deepseek()`](https://ellmer.tidyverse.org/dev/reference/chat_deepseek.md)
  : Chat with a model hosted on DeepSeek
- [`chat_github()`](https://ellmer.tidyverse.org/dev/reference/chat_github.md)
  [`models_github()`](https://ellmer.tidyverse.org/dev/reference/chat_github.md)
  : Chat with a model hosted on the GitHub model marketplace
- [`chat_google_gemini()`](https://ellmer.tidyverse.org/dev/reference/chat_google_gemini.md)
  [`chat_google_vertex()`](https://ellmer.tidyverse.org/dev/reference/chat_google_gemini.md)
  [`models_google_gemini()`](https://ellmer.tidyverse.org/dev/reference/chat_google_gemini.md)
  [`models_google_vertex()`](https://ellmer.tidyverse.org/dev/reference/chat_google_gemini.md)
  **\[experimental\]** : Chat with a Google Gemini or Vertex AI model
- [`chat_groq()`](https://ellmer.tidyverse.org/dev/reference/chat_groq.md)
  : Chat with a model hosted on Groq
- [`chat_huggingface()`](https://ellmer.tidyverse.org/dev/reference/chat_huggingface.md)
  : Chat with a model hosted on Hugging Face Serverless Inference API
- [`chat_mistral()`](https://ellmer.tidyverse.org/dev/reference/chat_mistral.md)
  [`models_mistral()`](https://ellmer.tidyverse.org/dev/reference/chat_mistral.md)
  : Chat with a model hosted on Mistral's La Platforme
- [`chat_ollama()`](https://ellmer.tidyverse.org/dev/reference/chat_ollama.md)
  [`models_ollama()`](https://ellmer.tidyverse.org/dev/reference/chat_ollama.md)
  : Chat with a local Ollama model
- [`chat_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_openai.md)
  [`models_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_openai.md)
  : Chat with an OpenAI model
- [`chat_openai_compatible()`](https://ellmer.tidyverse.org/dev/reference/chat_openai_compatible.md)
  : Chat with an OpenAI-compatible model
- [`chat_openrouter()`](https://ellmer.tidyverse.org/dev/reference/chat_openrouter.md)
  : Chat with one of the many models hosted on OpenRouter
- [`chat_perplexity()`](https://ellmer.tidyverse.org/dev/reference/chat_perplexity.md)
  : Chat with a model hosted on perplexity.ai
- [`chat_portkey()`](https://ellmer.tidyverse.org/dev/reference/chat_portkey.md)
  [`models_portkey()`](https://ellmer.tidyverse.org/dev/reference/chat_portkey.md)
  : Chat with a model hosted on PortkeyAI
- [`chat_snowflake()`](https://ellmer.tidyverse.org/dev/reference/chat_snowflake.md)
  : Chat with a model hosted on Snowflake
- [`chat_vllm()`](https://ellmer.tidyverse.org/dev/reference/chat_vllm.md)
  [`models_vllm()`](https://ellmer.tidyverse.org/dev/reference/chat_vllm.md)
  : Chat with a model hosted by vLLM
- [`Chat`](https://ellmer.tidyverse.org/dev/reference/Chat.md) : The
  Chat object
- [`token_usage()`](https://ellmer.tidyverse.org/dev/reference/token_usage.md)
  : Report on token usage in the current session

### Provider-specific helpers

- [`google_upload()`](https://ellmer.tidyverse.org/dev/reference/google_upload.md)
  **\[experimental\]** : Upload a file to gemini
- [`claude_file_upload()`](https://ellmer.tidyverse.org/dev/reference/claude_file_upload.md)
  [`claude_file_list()`](https://ellmer.tidyverse.org/dev/reference/claude_file_upload.md)
  [`claude_file_get()`](https://ellmer.tidyverse.org/dev/reference/claude_file_upload.md)
  [`claude_file_download()`](https://ellmer.tidyverse.org/dev/reference/claude_file_upload.md)
  [`claude_file_delete()`](https://ellmer.tidyverse.org/dev/reference/claude_file_upload.md)
  **\[experimental\]** : Upload, downloand, and manage files for Claude
- [`claude_tool_web_search()`](https://ellmer.tidyverse.org/dev/reference/claude_tool_web_search.md)
  : Claude web search tool
- [`claude_tool_web_fetch()`](https://ellmer.tidyverse.org/dev/reference/claude_tool_web_fetch.md)
  : Claude web fetch tool
- [`google_tool_web_search()`](https://ellmer.tidyverse.org/dev/reference/google_tool_web_search.md)
  : Google web search (grounding) tool
- [`google_tool_web_fetch()`](https://ellmer.tidyverse.org/dev/reference/google_tool_web_fetch.md)
  : Google URL fetch tool
- [`openai_tool_web_search()`](https://ellmer.tidyverse.org/dev/reference/openai_tool_web_search.md)
  : OpenAI web search tool

## Chat helpers

- [`create_tool_def()`](https://ellmer.tidyverse.org/dev/reference/create_tool_def.md)
  : Create metadata for a tool
- [`content_image_url()`](https://ellmer.tidyverse.org/dev/reference/content_image_url.md)
  [`content_image_file()`](https://ellmer.tidyverse.org/dev/reference/content_image_url.md)
  [`content_image_plot()`](https://ellmer.tidyverse.org/dev/reference/content_image_url.md)
  : Encode images for chat input
- [`content_pdf_file()`](https://ellmer.tidyverse.org/dev/reference/content_pdf_file.md)
  [`content_pdf_url()`](https://ellmer.tidyverse.org/dev/reference/content_pdf_file.md)
  : Encode PDFs content for chat input
- [`live_console()`](https://ellmer.tidyverse.org/dev/reference/live_console.md)
  [`live_browser()`](https://ellmer.tidyverse.org/dev/reference/live_console.md)
  : Open a live chat application
- [`interpolate()`](https://ellmer.tidyverse.org/dev/reference/interpolate.md)
  [`interpolate_file()`](https://ellmer.tidyverse.org/dev/reference/interpolate.md)
  [`interpolate_package()`](https://ellmer.tidyverse.org/dev/reference/interpolate.md)
  : Helpers for interpolating data into prompts

## Parallel and batch chat

- [`batch_chat()`](https://ellmer.tidyverse.org/dev/reference/batch_chat.md)
  [`batch_chat_text()`](https://ellmer.tidyverse.org/dev/reference/batch_chat.md)
  [`batch_chat_structured()`](https://ellmer.tidyverse.org/dev/reference/batch_chat.md)
  [`batch_chat_completed()`](https://ellmer.tidyverse.org/dev/reference/batch_chat.md)
  : Submit multiple chats in one batch
- [`parallel_chat()`](https://ellmer.tidyverse.org/dev/reference/parallel_chat.md)
  [`parallel_chat_text()`](https://ellmer.tidyverse.org/dev/reference/parallel_chat.md)
  [`parallel_chat_structured()`](https://ellmer.tidyverse.org/dev/reference/parallel_chat.md)
  : Submit multiple chats in parallel

## Tools and structured data

- [`tool()`](https://ellmer.tidyverse.org/dev/reference/tool.md) :
  Define a tool
- [`tool_annotations()`](https://ellmer.tidyverse.org/dev/reference/tool_annotations.md)
  : Tool annotations
- [`tool_reject()`](https://ellmer.tidyverse.org/dev/reference/tool_reject.md)
  : Reject a tool call
- [`type_boolean()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md)
  [`type_integer()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md)
  [`type_number()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md)
  [`type_string()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md)
  [`type_enum()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md)
  [`type_array()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md)
  [`type_object()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md)
  [`type_from_schema()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md)
  [`type_ignore()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md)
  : Type specifications

## Objects

These classes abstract away behaviour differences in chat providers so
that for typical ellmer use you don’t need to worry about them. You’ll
need to learn more about the objects if you’re doing something that’s
only supported by one provider, or if you’re implementing a new
provider.

- [`Provider()`](https://ellmer.tidyverse.org/dev/reference/Provider.md)
  : A chatbot provider
- [`Turn()`](https://ellmer.tidyverse.org/dev/reference/Turn.md)
  [`UserTurn()`](https://ellmer.tidyverse.org/dev/reference/Turn.md)
  [`SystemTurn()`](https://ellmer.tidyverse.org/dev/reference/Turn.md)
  [`AssistantTurn()`](https://ellmer.tidyverse.org/dev/reference/Turn.md)
  : A user, assistant, or system turn
- [`Content()`](https://ellmer.tidyverse.org/dev/reference/Content.md)
  [`ContentText()`](https://ellmer.tidyverse.org/dev/reference/Content.md)
  [`ContentImage()`](https://ellmer.tidyverse.org/dev/reference/Content.md)
  [`ContentImageRemote()`](https://ellmer.tidyverse.org/dev/reference/Content.md)
  [`ContentImageInline()`](https://ellmer.tidyverse.org/dev/reference/Content.md)
  [`ContentToolRequest()`](https://ellmer.tidyverse.org/dev/reference/Content.md)
  [`ContentToolResult()`](https://ellmer.tidyverse.org/dev/reference/Content.md)
  [`ContentThinking()`](https://ellmer.tidyverse.org/dev/reference/Content.md)
  [`ContentPDF()`](https://ellmer.tidyverse.org/dev/reference/Content.md)
  : Content types received from and sent to a chatbot
- [`TypeBasic()`](https://ellmer.tidyverse.org/dev/reference/Type.md)
  [`TypeEnum()`](https://ellmer.tidyverse.org/dev/reference/Type.md)
  [`TypeArray()`](https://ellmer.tidyverse.org/dev/reference/Type.md)
  [`TypeJsonSchema()`](https://ellmer.tidyverse.org/dev/reference/Type.md)
  [`TypeIgnore()`](https://ellmer.tidyverse.org/dev/reference/Type.md)
  [`TypeObject()`](https://ellmer.tidyverse.org/dev/reference/Type.md) :
  Type definitions for function calling and structured data extraction.

## Utilities

- [`contents_text()`](https://ellmer.tidyverse.org/dev/reference/contents_text.md)
  [`contents_html()`](https://ellmer.tidyverse.org/dev/reference/contents_text.md)
  [`contents_markdown()`](https://ellmer.tidyverse.org/dev/reference/contents_text.md)
  **\[experimental\]** : Format contents into a textual representation
- [`contents_record()`](https://ellmer.tidyverse.org/dev/reference/contents_record.md)
  [`contents_replay()`](https://ellmer.tidyverse.org/dev/reference/contents_record.md)
  : Record and replay content
- [`df_schema()`](https://ellmer.tidyverse.org/dev/reference/df_schema.md)
  : Describe the schema of a data frame, suitable for sending to an LLM
- [`params()`](https://ellmer.tidyverse.org/dev/reference/params.md) :
  Standard model parameters
