#' A chatbot model
#'
#' A Model captures the details of a specific language model and its
#' configuration parameters. This includes the model name, parameters like
#' temperature and max_tokens, and any extra arguments to include in API
#' requests.
#'
#' Model objects are typically created internally by [chat_openai()],
#' [chat_anthropic()], and other `chat_*()` functions. You generally won't
#' need to create them directly unless you're implementing a custom provider.
#'
#' @export
#' @param name Name of the model (e.g., "gpt-4", "claude-sonnet-4").
#' @param params A list of standard parameters created by [params()].
#' @param extra_args Arbitrary extra arguments to be included in the request body.
#' @return An S7 Model object.
#' @examples
#' Model(
#'   name = "gpt-4",
#'   params = params(temperature = 0.7)
#' )
Model <- new_class(
  "Model",
  properties = list(
    name = prop_string(),
    params = class_list,
    extra_args = class_list
  )
)
