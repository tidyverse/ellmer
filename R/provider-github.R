#' Chat with a model hosted on the GitHub model marketplace
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `chat_github()` and `models_github()` are defunct because GitHub Models
#' was retired on 2026-07-30.
#'
#' @family chatbots
#' @param api_key `r lifecycle::badge("deprecated")` Use `credentials` instead.
#' @param credentials `r api_key_param("GITHUB_PAT")`
#' @param model `r param_model("gpt-5")`
#' @param params Common model parameters, usually created by [params()].
#' @export
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @examples
#' \dontrun{
#' chat <- chat_github()
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_github <- function(
  system_prompt = NULL,
  base_url = "https://models.github.ai/inference/",
  api_key = NULL,
  credentials = NULL,
  model = NULL,
  params = NULL,
  api_args = list(),
  echo = NULL,
  api_headers = character()
) {
  lifecycle::deprecate_stop(
    "0.5.0",
    "chat_github()",
    details = c(
      "GitHub Models was retired on 2026-07-30.",
      i = "`chat_google_gemini()` offers a free tier and `chat_posit()` offers a free trial."
    )
  )
}

#' @rdname chat_github
#' @export
models_github <- function(
  base_url = "https://models.github.ai/",
  api_key = NULL,
  credentials = NULL
) {
  lifecycle::deprecate_stop(
    "0.5.0",
    "models_github()",
    details = c(
      "GitHub Models was retired on 2026-07-30.",
      i = "`chat_google_gemini()` offers a free tier and `chat_posit()` offers a free trial."
    )
  )
}
