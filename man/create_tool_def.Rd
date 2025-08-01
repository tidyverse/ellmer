% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tools-def-auto.R
\name{create_tool_def}
\alias{create_tool_def}
\title{Create metadata for a tool}
\usage{
create_tool_def(
  topic,
  chat = NULL,
  model = deprecated(),
  echo = interactive(),
  verbose = FALSE
)
}
\arguments{
\item{topic}{A symbol or string literal naming the function to create
metadata for. Can also be an expression of the form \code{pkg::fun}.}

\item{chat}{A \code{Chat} object used to generate the output. If \code{NULL}
(the default) uses \code{\link[=chat_openai]{chat_openai()}}.}

\item{model}{\code{lifecycle::badge("deprecated")} Formally used for definining
the model used by the chat. Now supply \code{chat} instead.}

\item{echo}{Emit the registration code to the console. Defaults to \code{TRUE} in
interactive sessions.}

\item{verbose}{If \code{TRUE}, print the input we send to the LLM, which may be
useful for debugging unexpectedly poor results.}
}
\value{
A \code{register_tool} call that you can copy and paste into your code.
Returned invisibly if \code{echo} is \code{TRUE}.
}
\description{
In order to use a function as a tool in a chat, you need to craft the right
call to \code{\link[=tool]{tool()}}. This function helps you do that for documented functions by
extracting the function's R documentation and using an LLM to generate the
\code{tool()} call. It's meant to be used interactively while writing your
code, not as part of your final code.

If the function has package documentation, that will be used. Otherwise, if
the source code of the function can be automatically detected, then the
comments immediately preceding the function are used (especially helpful if
those are roxygen2 comments). If neither are available, then just the function
signature is used.

Note that this function is inherently imperfect. It can't handle all possible
R functions, because not all parameters are suitable for use in a tool call
(for example, because they're not serializable to simple JSON objects). The
documentation might not specify the expected shape of arguments to the level
of detail that would allow an exact JSON schema to be generated. Please be
sure to review the generated code before using it!
}
\examples{
\dontrun{
  # These are all equivalent
  create_tool_def(rnorm)
  create_tool_def(stats::rnorm)
  create_tool_def("rnorm")
  create_tool_def("rnorm", chat = chat_azure_openai())
}

}
