prop_string <- function(default = NULL, allow_null = FALSE, allow_na = FALSE) {
  force(allow_null)
  force(allow_na)

  new_property(
    class = if (allow_null) NULL | class_character else class_character,
    default = if (is.null(default) && !allow_null) {
      quote(stop("Required"))
    } else {
      default
    },
    validator = function(value) {
      if (allow_null && is.null(value)) {
        return()
      }

      if (length(value) != 1) {
        paste0("must be a single string, not ", obj_type_friendly(value), ".")
      } else if (!allow_na && is.na(value)) {
        "must not be missing."
      }
    }
  )
}

# Deprecated Provider properties, backed by a hidden ".model" attribute that
# Chat sets. Remove once the deprecation cycle is complete (#1098).
prop_deprecated <- function(name, model_prop) {
  force(name)
  force(model_prop)

  new_property(
    class = class_any,
    getter = function(self) {
      user_env <- deprecated_prop_user_env()
      lifecycle::deprecate_warn(
        "0.5.0",
        I(paste0("`Provider@", name, "`")),
        details = "Model details now live on the `Model` object; see `chat$get_model_object()`.",
        user_env = user_env
      )
      model <- provider_model(self)
      if (is.null(model)) NULL else prop(model, model_prop)
    },
    setter = function(self, value) {
      if (is.null(value)) {
        return(self)
      }
      user_env <- deprecated_prop_user_env()
      lifecycle::deprecate_warn(
        "0.5.0",
        I(paste0("`Provider(", name, ")`")),
        details = "Model details now live on the `Model` object.",
        user_env = user_env
      )
      model <- provider_model(self) %||% Model(name = "")
      prop(model, model_prop) <- value
      provider_model(self) <- model
      self
    }
  )
}

# The getter/setter are called from S7's `@` dispatch, so lifecycle's default
# would blame S7. Walk up the stack to the first frame outside S7 and ellmer.
deprecated_prop_user_env <- function() {
  skip <- list(ns_env("ellmer"), ns_env("S7"))
  for (i in rev(seq_len(sys.nframe() - 1))) {
    env <- sys.frame(i)
    if (!any(vapply(skip, identical, logical(1), topenv(env)))) {
      return(env)
    }
  }
  global_env()
}

provider_model <- function(provider) {
  attr(provider, ".model", exact = TRUE)
}

`provider_model<-` <- function(provider, value) {
  attr(provider, ".model") <- value
  provider
}

prop_bool <- function(default, allow_null = FALSE, allow_na = FALSE) {
  force(allow_null)
  force(allow_na)

  new_property(
    class = if (allow_null) NULL | class_logical else class_logical,
    default = default,
    validator = function(value) {
      if (allow_null && is.null(value)) {
        return()
      }

      if (length(value) != 1) {
        if (allow_na) {
          paste0(
            "must be a single TRUE or FALSE, not ",
            obj_type_friendly(value),
            "."
          )
        } else {
          paste0(
            "must be a single TRUE, FALSE or NA, not ",
            obj_type_friendly(value),
            "."
          )
        }
      } else if (!allow_na && is.na(value)) {
        paste0("must be a TRUE or FALSE, not NA.")
      }
    }
  )
}

prop_list_of <- function(class, names = c("any", "all", "none")) {
  force(class)
  names <- arg_match(names)

  new_property(
    class = class_list,
    validator = function(value) {
      for (i in seq_along(value)) {
        val <- value[[i]]
        if (!S7_inherits(val, class)) {
          return(paste0(
            "must be a list of <",
            class@name,
            ">s. ",
            "Element ",
            i,
            " is ",
            obj_type_friendly(val),
            "."
          ))
        }
      }
      if (names == "all" && any(names2(value) == "")) {
        "must be a named list."
      } else if (names == "none" && any(names2(value) != "")) {
        "must be an unnamed list."
      }
    }
  )
}

prop_number_whole <- function(
  default = NULL,
  min = NULL,
  max = NULL,
  allow_null = FALSE,
  allow_na = FALSE
) {
  force(allow_null)
  force(allow_na)

  new_property(
    class = if (allow_null) NULL | class_double else class_double,
    default = default,
    validator = function(value) {
      if (allow_null && is.null(value)) {
        return()
      }

      if (length(value) != 1) {
        paste0("must be a whole number, not ", obj_type_friendly(value), ".")
      } else if (!allow_na && is.na(value)) {
        "must not be missing."
      } else if (value != trunc(value)) {
        paste0("must be a whole number, not ", obj_type_friendly(value), ".")
      } else if (!is.null(min) && value < min) {
        paste0("must be at least ", min, ", not ", value, ".")
      } else if (!is.null(max) && value > max) {
        paste0("must be at most ", max, ", not ", value, ".")
      }
    }
  )
}

prop_number_decimal <- function(
  default = NULL,
  min = NULL,
  max = NULL,
  allow_null = FALSE,
  allow_na = FALSE,
  allow_infinite = FALSE
) {
  force(allow_null)
  force(allow_na)

  new_property(
    class = if (allow_null) NULL | class_double else class_double,
    default = default,
    validator = function(value) {
      if (allow_null && is.null(value)) {
        return()
      }

      if (length(value) != 1) {
        paste0("must be a whole number, not ", obj_type_friendly(value), ".")
      } else if (!allow_infinite && is.infinite(value)) {
        paste0("must be finite, not ", value, ".")
      } else if (!is.null(min) && value < min) {
        paste0("must be at least ", min, ", not ", value, ".")
      } else if (!is.null(max) && value > max) {
        paste0("must be at most ", max, ", not ", value, ".")
      } else if (!allow_na && is.na(value)) {
        "must not be missing."
      }
    }
  )
}

# Weakrefs are not serialised, so we can use them to ensure that a value
# is never recorded by saveRDS() and friends.
prop_redacted <- function(name, default = NULL, allow_null = FALSE) {
  force(allow_null)

  # Tie all weakref values to the enclosing environment of `new_property()`
  # This ensures that all the weakrefs will go out of scope and be gc'd if
  # the class is deleted.
  key <- environment()

  new_property(
    name = name,
    default = if (is.null(default) && !allow_null) {
      quote(stop("Required"))
    } else {
      default
    },
    getter = function(self) {
      wref_value(prop(self, name))
    },
    setter = function(self, value) {
      check_string(value, allow_null = allow_null)

      prop(self, name) <- new_weakref(key, value)
      self
    }
  )
}
