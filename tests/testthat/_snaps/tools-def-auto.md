# roxygen2 comment extraction works

    Code
      extract_comments_and_signature(has_roxygen_comments)
    Output
      [1] "#' A function for foo-ing three numbers.\n#'\n#' @param x The first param\n#' @param y The second param\n#' @param z Take a guess\n#' @returns The result of x %foo% y %foo% z.\nfunction (x, y, z = pi - 3.14)  ..."

---

    Code
      extract_comments_and_signature(aliased_function)
    Output
      [1] "#' A function for foo-ing three numbers.\n#'\n#' @param x The first param\n#' @param y The second param\n#' @param z Take a guess\n#' @returns The result of x %foo% y %foo% z.\nfunction (x, y, z = pi - 3.14)  ..."

---

    Code
      extract_comments_and_signature(indented_comments)
    Output
      [1] "  #' A function for foo-ing three numbers.\n  #'\n  #' @param x The first param\n  #' @param y The second param\n  #' @param z Take a guess\n  #' @returns The result of x %foo% y %foo% z.\nfunction (x, y, z = pi - 3.14)  ..."

---

    Code
      extract_comments_and_signature(no_srcfile)
    Output
      [1] "  #' A function for foo-ing three numbers.\nfunction (a, b, c = pi - 3.14)  ..."

# basic signature extraction works

    Code
      extract_comments_and_signature(no_roxygen_comments)
    Output
      [1] "function (i, j, k = pi - 3.14)  ..."

# checks its inputs

    Code
      create_tool_def(print, model = "gpt-4", chat = chat_google_gemini())
    Condition
      Error in `create_tool_def()`:
      ! Exactly one of `model` or `chat` must be supplied.
    Code
      create_tool_def(print, chat = 1)
    Condition
      Error in `create_tool_def()`:
      ! `chat` must be a <Chat> object or `NULL`, not the number 1.

# model is deprecated

    Code
      . <- create_tool_def(print, model = "gpt-4", echo = FALSE)
    Condition
      Warning:
      The `model` argument of `create_tool_def()` is deprecated as of ellmer 1.0.0.
      i Please use the `chat` argument instead.

