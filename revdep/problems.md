# btw (1.4.0)

* GitHub: <https://github.com/posit-dev/btw>
* Email: <mailto:garrick@adenbuie.com>
* GitHub mirror: <https://github.com/cran/btw>

Run `revdepcheck::cloud_details(, "btw")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
         'test-cli.R:479:3', 'test-cli.R:488:3', 'test-cli.R:501:3',
         'test-cli.R:515:3', 'test-cli.R:536:3', 'test-cli.R:548:3',
         'test-cli.R:566:3', 'test-cli.R:575:3', 'test-cli.R:583:3',
         'test-cli.R:605:3', 'test-cli.R:617:3', 'test-cli.R:623:3',
         'test-cli.R:634:3', 'test-cli.R:643:3'
       * {bsicons} is not installed (1): 'test-tool-agent-custom.R:533:3'
       * {phosphoricons} is not installed (1): 'test-tool-agent-custom.R:553:3'
       
       == Failed tests ================================================================
       -- Error ('test-tool-agent-subagent.R:85:3'): subagent_client() consults btw.md if options are unset --
       Error: Can't find property <ellmer::ProviderOpenRouter>@model
       Backtrace:
           x
        1. +-testthat::expect_equal(agent_client$get_provider()@model, "super-cool-model") at test-tool-agent-subagent.R:85:3
        2. | \-testthat::quasi_label(enquo(object), label)
        3. |   \-rlang::eval_bare(expr, quo_get_env(quo))
        4. +-agent_client$get_provider()@model
        5. +-S7:::`@.S7_object`(agent_client$get_provider(), model)
        6. \-S7 (local) `<fn>`("Can't find property %s@%s", `<ell::POR>`, "model")
       
       [ FAIL 1 | WARN 1 | SKIP 137 | PASS 1895 ]
       Error:
       ! Test failures.
       Execution halted
       Ran 8/8 deferred expressions
     ```

# GitAI (0.1.3)

* GitHub: <https://github.com/r-world-devs/GitAI>
* Email: <mailto:kamil.wais@gmail.com>
* GitHub mirror: <https://github.com/cran/GitAI>

Run `revdepcheck::cloud_details(, "GitAI")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
        5.       └─rlang::exec(provider_class, name = "mock", !!!provider_args) at ./setup.R:24:3
       ── Error ('test-set_llm.R:84:3'): setting LLM without system prompt ────────────
       Error in `(structure(function (name = stop("Required"), base_url = stop("Required"), extra_headers = character(0), credentials = function() NULL, preserve_thinking = FALSE, service_tier = character(0))  new_object(ProviderOpenAICompatible(name = name, base_url = base_url, extra_headers = extra_headers, credentials = credentials, preserve_thinking = preserve_thinking), service_tier = service_tier), name = "ProviderOpenAI", parent = structure(function (name = stop("Required"), base_url = stop("Required"), extra_headers = character(0), credentials = function() NULL, preserve_thinking = FALSE)  new_object(Provider(name = name, base_url = base_url, extra_headers = extra_headers, credentials = credentials), preserve_thinking = preserve_thinking), name = "ProviderOpenAICompatible", parent = structure(function (name = stop("Required"), base_url = stop("Required"), extra_headers = character(0), credentials = function() NULL)  {     name     base_url     extra_headers     credentials     new_object(S7_object(), name = name, base_url = base_url, extra_headers = extra_headers, credentials = credentials) }, name = "Provider", parent = structure(function ()  {     .Call(S7_object_) }, name = "S7_object", properties = list(), abstract = FALSE, constructor = function ()  {     .Call(S7_object_) }, validator = function (self)  {     if (!is_S7_type(self)) {         "Underlying data is corrupt"     } }, class = c("S7_class", "S7_object")), package = "ellmer", properties = list(name = structure(list(name = "name", class = structure(list(class = "character", constructor_name = "character", constructor = function (.data = character(0))  .data, validator = function (object)  {     if (base_class(object) != name) {         sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))     } }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = function (value)  {     if (allow_null && is.null(value)) {         return()     }     if (length(value) != 1) {         paste0("must be a single string, not ", obj_type_friendly(value), ".")     }     else if (!allow_na && is.na(value)) {         "must not be missing."     } }, default = stop("Required")), class = "S7_property"), base_url = structure(list(name = "base_url", class = structure(list(class = "character", constructor_name = "character", constructor = function (.data = character(0))  .data, validator = function (object)  {     if (base_class(object) != name) {         sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))     } }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = function (value)  {     if (allow_null && is.null(value)) {         return()     }     if (length(value) != 1) {         paste0("must be a single string, not ", obj_type_friendly(value), ".")     }     else if (!allow_na && is.na(value)) {         "must not be missing."     } }, default = stop("Required")), class = "S7_property"), extra_headers = structure(list(name = "extra_headers", class = structure(list(class = "character", constructor_name = "character", constructor = function (.data = character(0))  .data, validator = function (object)  {     if (base_class(object) != name) {         sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))     } }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = NULL, default = NULL), class = "S7_property"), credentials = structure(list(name = "credentials", class = structure(list(classes = list(structure(list(class = "function", constructor_name = "fun", constructor = function (.data = function() NULL)  .data, validator = function (object)  {     if (base_class(object) != name) {         sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))     } }), class = "S7_base_class"), NULL)), class = "S7_union"), getter = NULL, setter = NULL, validator = NULL, default = NULL), class = "S7_property")), abstract = FALSE, constructor = function (name = stop("Required"), base_url = stop("Required"), extra_headers = character(0), credentials = function() NULL)  {     name     base_url     extra_headers     credentials     new_object(S7_object(), name = name, base_url = base_url, extra_headers = extra_headers, credentials = credentials) }, class = c("S7_class", "S7_object")), package = "ellmer", properties = list(name = structure(list(name = "name", class = structure(list(class = "character", constructor_name = "character", constructor = function (.data = character(0))  .data, validator = function (object)  {     if (base_class(object) != name) {         sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))     } }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = function (value)  {     if (allow_null && is.null(value)) {         return()     }     if (length(value) != 1) {         paste0("must be a single string, not ", obj_type_friendly(value), ".")     }     else if (!allow_na && is.na(value)) {         "must not be missing."     } }, default = stop("Required")), class = "S7_property"), base_url = structure(list(name = "base_url", class = structure(list(class = "character", constructor_name = "character", constructor = function (.data = character(0))  .data, validator = function (object)  {     if (base_class(object) != name) {         sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))     } }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = function (value)  {     if (allow_null && is.null(value)) {         return()     }     if (length(value) != 1) {         paste0("must be a single string, not ", obj_type_friendly(value), ".")     }     else if (!allow_na && is.na(value)) {         "must not be missing."     } }, default = stop("Required")), class = "S7_property"), extra_headers = structure(list(name = "extra_headers", class = structure(list(class = "character", constructor_name = "character", constructor = function (.data = character(0))  .data, validator = function (object)  {     if (base_class(object) != name) {         sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))     } }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = NULL, default = NULL), class = "S7_property"), credentials = structure(list(name = "credentials", class = structure(list(classes = list(structure(list(class = "function", constructor_name = "fun", constructor = function (.data = function() NULL)  .data, validator = function (object)  {     if (base_class(object) != name) {         sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))     } }), class = "S7_base_class"), NULL)), class = "S7_union"), getter = NULL, setter = NULL, validator = NULL, default = NULL), class = "S7_property"), preserve_thinking = structure(list(name = "preserve_thinking", class = structure(list(class = "logical", constructor_name = "logical", constructor = function (.data = logical(0))  .data, validator = function (object)  {     if (base_class(object) != name) {         sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))     } }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = NULL, default = FALSE), class = "S7_property")), abstract = FALSE, constructor = function (name = stop("Required"), base_url = stop("Required"), extra_headers = character(0), credentials = function() NULL, preserve_thinking = FALSE)  new_object(Provider(name = name, base_url = base_url, extra_headers = extra_headers, credentials = credentials), preserve_thinking = preserve_thinking), class = c("S7_class", "S7_object")), package = "ellmer", properties = list(name = structure(list(name = "name", class = structure(list(class = "character", constructor_name = "character", constructor = function (.data = character(0))  .data, validator = function (object)  {     if (base_class(object) != name) {         sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))     } }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = function (value)  {     if (allow_null && is.null(value)) {         return()     }     if (length(value) != 1) {         paste0("must be a single string, not ", obj_type_friendly(value), ".")     }     else if (!allow_na && is.na(value)) {         "must not be missing."     } }, default = stop("Required")), class = "S7_property"), base_url = structure(list(name = "base_url", class = structure(list(class = "character", constructor_name = "character", constructor = function (.data = character(0))  .data, validator = function (object)  {     if (base_class(object) != name) {         sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))     } }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = function (value)  {     if (allow_null && is.null(value)) {         return()     }     if (length(value) != 1) {         paste0("must be a single string, not ", obj_type_friendly(value), ".")     }     else if (!allow_na && is.na(value)) {         "must not be missing."     } }, default = stop("Required")), class = "S7_property"), extra_headers = structure(list(name = "extra_headers", class = structure(list(class = "character", constructor_name = "character", constructor = function (.data = character(0))  .data, validator = function (object)  {     if (base_class(object) != name) {         sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))     } }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = NULL, default = NULL), class = "S7_property"), credentials = structure(list(name = "credentials", class = structure(list(classes = list(structure(list(class = "function", constructor_name = "fun", constructor = function (.data = function() NULL)  .data, validator = function (object)  {     if (base_class(object) != name) {         sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))     } }), class = "S7_base_class"), NULL)), class = "S7_union"), getter = NULL, setter = NULL, validator = NULL, default = NULL), class = "S7_property"), preserve_thinking = structure(list(name = "preserve_thinking", class = structure(list(class = "logical", constructor_name = "logical", constructor = function (.data = logical(0))  .data, validator = function (object)  {     if (base_class(object) != name) {         sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))     } }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = NULL, default = FALSE), class = "S7_property"), service_tier = structure(list(name = "service_tier", class = structure(list(class = "character", constructor_name = "character", constructor = function (.data = character(0))  .data, validator = function (object)  {     if (base_class(object) != name) {         sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))     } }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = NULL, default = NULL), class = "S7_property")), abstract = FALSE, constructor = function (name = stop("Required"), base_url = stop("Required"), extra_headers = character(0), credentials = function() NULL, preserve_thinking = FALSE, service_tier = character(0))  new_object(ProviderOpenAICompatible(name = name, base_url = base_url, extra_headers = extra_headers, credentials = credentials, preserve_thinking = preserve_thinking), service_tier = service_tier), class = c("S7_class", "S7_object")))(name = "mock", base_url = "https://api.mocked.com/v1", model = "gpt-4o-mini", params = list(seed = 1014), extra_args = list(), credentials = function ()  api_key)`: unused arguments (model = "gpt-4o-mini", params = list(1014), extra_args = list())
       Backtrace:
           ▆
        1. └─GitAI::set_llm(initialize_project("gitai_test_project")) at test-set_llm.R:84:3
        2.   ├─rlang::exec(provider_method, !!!provider_args)
        3.   └─GitAI (local) `<fn>`(model = "gpt-4o-mini", params = NULL, echo = "none")
        4.     └─GitAI:::mock_chat_method(...) at ./setup.R:50:3
        5.       └─rlang::exec(provider_class, name = "mock", !!!provider_args) at ./setup.R:24:3
       ── Error ('test-set_llm.R:100:3'): setting system prompt ───────────────────────
       Error in `(structure(function (name = stop("Required"), base_url = stop("Required"), extra_headers = character(0), credentials = function() NULL, preserve_thinking = FALSE, service_tier = character(0))  new_object(ProviderOpenAICompatible(name = name, base_url = base_url, extra_headers = extra_headers, credentials = credentials, preserve_thinking = preserve_thinking), service_tier = service_tier), name = "ProviderOpenAI", parent = structure(function (name = stop("Required"), base_url = stop("Required"), extra_headers = character(0), credentials = function() NULL, preserve_thinking = FALSE)  new_object(Provider(name = name, base_url = base_url, extra_headers = extra_headers, credentials = credentials), preserve_thinking = preserve_thinking), name = "ProviderOpenAICompatible", parent = structure(function (name = stop("Required"), base_url = stop("Required"), extra_headers = character(0), credentials = function() NULL)  {     name     base_url     extra_headers     credentials     new_object(S7_object(), name = name, base_url = base_url, extra_headers = extra_headers, credentials = credentials) }, name = "Provider", parent = structure(function ()  {     .Call(S7_object_) }, name = "S7_object", properties = list(), abstract = FALSE, constructor = function ()  {     .Call(S7_object_) }, validator = function (self)  {     if (!is_S7_type(self)) {         "Underlying data is corrupt"     } }, class = c("S7_class", "S7_object")), package = "ellmer", properties = list(name = structure(list(name = "name", class = structure(list(class = "character", constructor_name = "character", constructor = function (.data = character(0))  .data, validator = function (object)  {     if (base_class(object) != name) {         sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))     } }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = function (value)  {     if (allow_null && is.null(value)) {         return()     }     if (length(value) != 1) {         paste0("must be a single string, not ", obj_type_friendly(value), ".")     }     else if (!allow_na && is.na(value)) {         "must not be missing."     } }, default = stop("Required")), class = "S7_property"), base_url = structure(list(name = "base_url", class = structure(list(class = "character", constructor_name = "character", constructor = function (.data = character(0))  .data, validator = function (object)  {     if (base_class(object) != name) {         sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))     } }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = function (value)  {     if (allow_null && is.null(value)) {         return()     }     if (length(value) != 1) {         paste0("must be a single string, not ", obj_type_friendly(value), ".")     }     else if (!allow_na && is.na(value)) {         "must not be missing."     } }, default = stop("Required")), class = "S7_property"), extra_headers = structure(list(name = "extra_headers", class = structure(list(class = "character", constructor_name = "character", constructor = function (.data = character(0))  .data, validator = function (object)  {     if (base_class(object) != name) {         sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))     } }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = NULL, default = NULL), class = "S7_property"), credentials = structure(list(name = "credentials", class = structure(list(classes = list(structure(list(class = "function", constructor_name = "fun", constructor = function (.data = function() NULL)  .data, validator = function (object)  {     if (base_class(object) != name) {         sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))     } }), class = "S7_base_class"), NULL)), class = "S7_union"), getter = NULL, setter = NULL, validator = NULL, default = NULL), class = "S7_property")), abstract = FALSE, constructor = function (name = stop("Required"), base_url = stop("Required"), extra_headers = character(0), credentials = function() NULL)  {     name     base_url     extra_headers     credentials     new_object(S7_object(), name = name, base_url = base_url, extra_headers = extra_headers, credentials = credentials) }, class = c("S7_class", "S7_object")), package = "ellmer", properties = list(name = structure(list(name = "name", class = structure(list(class = "character", constructor_name = "character", constructor = function (.data = character(0))  .data, validator = function (object)  {     if (base_class(object) != name) {         sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))     } }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = function (value)  {     if (allow_null && is.null(value)) {         return()     }     if (length(value) != 1) {         paste0("must be a single string, not ", obj_type_friendly(value), ".")     }     else if (!allow_na && is.na(value)) {         "must not be missing."     } }, default = stop("Required")), class = "S7_property"), base_url = structure(list(name = "base_url", class = structure(list(class = "character", constructor_name = "character", constructor = function (.data = character(0))  .data, validator = function (object)  {     if (base_class(object) != name) {         sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))     } }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = function (value)  {     if (allow_null && is.null(value)) {         return()     }     if (length(value) != 1) {         paste0("must be a single string, not ", obj_type_friendly(value), ".")     }     else if (!allow_na && is.na(value)) {         "must not be missing."     } }, default = stop("Required")), class = "S7_property"), extra_headers = structure(list(name = "extra_headers", class = structure(list(class = "character", constructor_name = "character", constructor = function (.data = character(0))  .data, validator = function (object)  {     if (base_class(object) != name) {         sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))     } }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = NULL, default = NULL), class = "S7_property"), credentials = structure(list(name = "credentials", class = structure(list(classes = list(structure(list(class = "function", constructor_name = "fun", constructor = function (.data = function() NULL)  .data, validator = function (object)  {     if (base_class(object) != name) {         sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))     } }), class = "S7_base_class"), NULL)), class = "S7_union"), getter = NULL, setter = NULL, validator = NULL, default = NULL), class = "S7_property"), preserve_thinking = structure(list(name = "preserve_thinking", class = structure(list(class = "logical", constructor_name = "logical", constructor = function (.data = logical(0))  .data, validator = function (object)  {     if (base_class(object) != name) {         sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))     } }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = NULL, default = FALSE), class = "S7_property")), abstract = FALSE, constructor = function (name = stop("Required"), base_url = stop("Required"), extra_headers = character(0), credentials = function() NULL, preserve_thinking = FALSE)  new_object(Provider(name = name, base_url = base_url, extra_headers = extra_headers, credentials = credentials), preserve_thinking = preserve_thinking), class = c("S7_class", "S7_object")), package = "ellmer", properties = list(name = structure(list(name = "name", class = structure(list(class = "character", constructor_name = "character", constructor = function (.data = character(0))  .data, validator = function (object)  {     if (base_class(object) != name) {         sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))     } }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = function (value)  {     if (allow_null && is.null(value)) {         return()     }     if (length(value) != 1) {         paste0("must be a single string, not ", obj_type_friendly(value), ".")     }     else if (!allow_na && is.na(value)) {         "must not be missing."     } }, default = stop("Required")), class = "S7_property"), base_url = structure(list(name = "base_url", class = structure(list(class = "character", constructor_name = "character", constructor = function (.data = character(0))  .data, validator = function (object)  {     if (base_class(object) != name) {         sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))     } }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = function (value)  {     if (allow_null && is.null(value)) {         return()     }     if (length(value) != 1) {         paste0("must be a single string, not ", obj_type_friendly(value), ".")     }     else if (!allow_na && is.na(value)) {         "must not be missing."     } }, default = stop("Required")), class = "S7_property"), extra_headers = structure(list(name = "extra_headers", class = structure(list(class = "character", constructor_name = "character", constructor = function (.data = character(0))  .data, validator = function (object)  {     if (base_class(object) != name) {         sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))     } }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = NULL, default = NULL), class = "S7_property"), credentials = structure(list(name = "credentials", class = structure(list(classes = list(structure(list(class = "function", constructor_name = "fun", constructor = function (.data = function() NULL)  .data, validator = function (object)  {     if (base_class(object) != name) {         sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))     } }), class = "S7_base_class"), NULL)), class = "S7_union"), getter = NULL, setter = NULL, validator = NULL, default = NULL), class = "S7_property"), preserve_thinking = structure(list(name = "preserve_thinking", class = structure(list(class = "logical", constructor_name = "logical", constructor = function (.data = logical(0))  .data, validator = function (object)  {     if (base_class(object) != name) {         sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))     } }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = NULL, default = FALSE), class = "S7_property"), service_tier = structure(list(name = "service_tier", class = structure(list(class = "character", constructor_name = "character", constructor = function (.data = character(0))  .data, validator = function (object)  {     if (base_class(object) != name) {         sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))     } }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = NULL, default = NULL), class = "S7_property")), abstract = FALSE, constructor = function (name = stop("Required"), base_url = stop("Required"), extra_headers = character(0), credentials = function() NULL, preserve_thinking = FALSE, service_tier = character(0))  new_object(ProviderOpenAICompatible(name = name, base_url = base_url, extra_headers = extra_headers, credentials = credentials, preserve_thinking = preserve_thinking), service_tier = service_tier), class = c("S7_class", "S7_object")))(name = "mock", base_url = "https://api.mocked.com/v1", model = "gpt-4o-mini", params = list(seed = 1014), extra_args = list(), credentials = function ()  api_key)`: unused arguments (model = "gpt-4o-mini", params = list(1014), extra_args = list())
       Backtrace:
           ▆
        1. ├─GitAI::set_prompt(set_llm(my_project), system_prompt = "You always return only 'Hi there!'") at test-set_llm.R:100:3
        2. └─GitAI::set_llm(my_project)
        3.   ├─rlang::exec(provider_method, !!!provider_args)
        4.   └─GitAI (local) `<fn>`(model = "gpt-4o-mini", params = NULL, echo = "none")
        5.     └─GitAI:::mock_chat_method(...) at ./setup.R:50:3
        6.       └─rlang::exec(provider_class, name = "mock", !!!provider_args) at ./setup.R:24:3
       
       [ FAIL 4 | WARN 0 | SKIP 6 | PASS 36 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# mini007 (0.4.0)

* GitHub: <https://github.com/feddelegrand7/mini007>
* Email: <mailto:ihaddaden.fodeil@gmail.com>
* GitHub mirror: <https://github.com/cran/mini007>

Run `revdepcheck::cloud_details(, "mini007")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > ## ------------------------------------------------
     > ## Method `Agent$new`
     > ## ------------------------------------------------
     > 
     >   # An API KEY is required in order to invoke the Agent
     >   openai_4_1_mini <- ellmer::chat(
     +     name = "openai/gpt-4.1-mini",
     +     api_key = Sys.getenv("OPENAI_API_KEY"),
     +     echo = "none"
     +   )
     Warning: The `api_key` argument of `chat_openai()` is deprecated as of ellmer 0.4.0.
     ℹ Please use the `credentials` argument instead.
     ℹ The deprecated feature was likely used in the ellmer package.
       Please report the issue at <https://github.com/tidyverse/ellmer/issues>.
     > 
     >   polar_bear_researcher <- Agent$new(
     +     name = "POLAR BEAR RESEARCHER",
     +     instruction = paste0(
     +     "You are an expert in polar bears, ",
     +     "you task is to collect information about polar bears. Answer in 1 sentence max."
     +     ),
     +     llm_object = openai_4_1_mini
     +   )
     Error: Can't find property <ellmer::ProviderOpenAI>@model
     Execution halted
     ```

# querychat (0.3.0)

* GitHub: <https://github.com/posit-dev/querychat>
* Email: <mailto:garrick@posit.co>
* GitHub mirror: <https://github.com/cran/querychat>

Run `revdepcheck::cloud_details(, "querychat")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
         'test-DBISource.R:41:3', 'test-DataFrameSource.R:15:3',
         'test-DataFrameSource.R:24:3', 'test-DataFrameSource.R:153:5',
         'test-DataSource.R:2:3', 'test-DataSource.R:388:3', 'test-QueryChat.R:81:3',
         'test-QueryChat.R:656:1', 'test-QueryChat.R:829:3',
         'test-querychat_tools.R:1:1', 'test-querychat_tools.R:14:1',
         'test-querychat_tools.R:18:1', 'test-TblSqlSource.R:12:3'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test-QueryChat.R:637:3'): QueryChat$generate_greeting() generates a greeting using the LLM client ──
       <evalError/missingArgError/error/condition>
       Error in `initialize(...)`: argument "model" is missing, with no default
       Backtrace:
           ▆
        1. └─querychat:::mock_ellmer_chat_client(...) at test-QueryChat.R:637:3
        2.   └─MockChat$new(ellmer::Provider("test", "test", "test")) at ./helper-fixtures.R:171:3
        3.     └─ellmer (local) initialize(...)
       ── Failure ('test-querychat_module.R:14:3'): Shiny app example loads without errors ──
       Expected `{ ... }` not to throw any errors.
       Actually got a <evalError> with message:
         argument "model" is missing, with no default
       
       [ FAIL 2 | WARN 2 | SKIP 15 | PASS 629 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# shidashi (0.2.0)

* GitHub: <https://github.com/dipterix/shidashi>
* Email: <mailto:dipterix.wang@gmail.com>
* GitHub mirror: <https://github.com/cran/shidashi>

Run `revdepcheck::cloud_details(, "shidashi")` for more info

## Newly broken

*   checking whether package ‘shidashi’ can be installed ... WARNING
     ```
     Found the following significant warnings:
       Note: possible error in 'ProviderAny(name = "mcp", ': unused argument (model = "unknown") 
     See ‘/tmp/workdir/shidashi/new/shidashi.Rcheck/00install.out’ for details.
     Information on the location(s) of code generating the ‘Note’s can be
     obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
     to ‘yes’.
     ```

*   checking R code for possible problems ... NOTE
     ```
     get_mcp_provider: possible error in ProviderAny(name = "mcp", model =
       "unknown", base_url = "http://localhost"): unused argument (model =
       "unknown")
     ```

# vitals (0.3.0)

* GitHub: <https://github.com/tidyverse/vitals>
* Email: <mailto:simon.couch@posit.co>
* GitHub mirror: <https://github.com/cran/vitals>

Run `revdepcheck::cloud_details(, "vitals")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
         8. │           └─vitals:::create_model_event(turn, sample, timestamp = timestamps$solve$started_at)
         9. │             ├─vitals:::drop_nulls(list(max_tokens = solver_chat$get_provider()@params$max_tokens))
        10. │             ├─solver_chat$get_provider()@params
        11. │             └─S7:::`@.S7_object`(solver_chat$get_provider(), params)
        12. └─S7 (local) `<fn>`("Can't find property %s@%s", `<el::POAI>`, "params")
       ── Error ('test-task.R:1139:3'): $log() respects dir argument ──────────────────
       Error: Can't find property <ellmer::ProviderOpenAI>@params
       Backtrace:
            ▆
         1. ├─tsk$log(dir = tmp_dir_arg) at test-task.R:1139:3
         2. │ ├─vitals:::eval_log(...)
         3. │ └─vitals:::translate_to_samples(...)
         4. │   └─vitals:::translate_to_sample(sample, scores = scores, timestamps = timestamps)
         5. │     └─vitals:::translate_to_events(sample = sample, timestamps = timestamps)
         6. │       └─vitals:::translate_events_solver(events, sample, timestamps = timestamps)
         7. │         └─vitals:::create_model_event(turn, sample, timestamp = timestamps$solve$started_at)
         8. │           ├─vitals:::drop_nulls(list(max_tokens = solver_chat$get_provider()@params$max_tokens))
         9. │           ├─solver_chat$get_provider()@params
        10. │           └─S7:::`@.S7_object`(solver_chat$get_provider(), params)
        11. └─S7 (local) `<fn>`("Can't find property %s@%s", `<el::POAI>`, "params")
       
       [ FAIL 21 | WARN 1 | SKIP 45 | PASS 88 ]
       Error:
       ! Test failures.
       Execution halted
     ```

