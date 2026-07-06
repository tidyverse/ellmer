#' @include utils.R
NULL

#' A model configuration
#'
#' A `Model` captures the details of a specific model: its name, standard
#' parameters, and any extra arguments to include in the API request body.
#' This is paired with a [Provider], which captures *who* you're talking to,
#' while the `Model` captures *what* you're asking for.
#'
#' You generally don't need to create `Model` objects directly; they are
#' created automatically by `chat_*()` functions like [chat_openai()] and
#' [chat_anthropic()].
#'
#' @export
#' @param name Name of the model (e.g. `"gpt-4.1"`, `"claude-sonnet-4-6"`).
#' @param params A list of standard parameters created by [params()].
#' @param extra_args Arbitrary extra arguments to be included in the request
#'   body.
#' @return An S7 Model object.
#' @examples
#' Model(name = "gpt-4.1")
#' Model(name = "claude-sonnet-4-6", params = params(temperature = 0))
Model <- new_class(
  "Model",
  properties = list(
    name = prop_string(),
    params = class_list,
    extra_args = class_list
  )
)
