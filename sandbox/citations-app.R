library(bslib)
library(ellmer)
library(shiny)
library(shinychat)

ui <- page_fillable(
  title = "Web citation rendering",
  chat_ui(
    "chat",
    height = "100%",
    placeholder = "Ask a question that needs current web sources...",
    enable_cancel = TRUE
  )
)

server <- function(input, output, session) {
  client <- chat_anthropic(
    system_prompt = paste(
      "You are a concise research assistant.",
      "Use web search for current facts and web fetch for supplied URLs.",
      "Cite every factual claim supported by a web source."
    ),
    beta_headers = "web-fetch-2025-09-10"
  )
  client$register_tool(claude_tool_web_search())
  client$register_tool(claude_tool_web_fetch(citations = TRUE))

  chat_server(
    "chat",
    client,
    greeting = paste(
      "Ask me to research a current topic, or summarize a URL.",
      "Search activity, fetched pages, inline citations, and sources",
      "will appear in the response."
    ),
    history = FALSE
  )
}

shinyApp(ui, server)
