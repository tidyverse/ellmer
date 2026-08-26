# deprecated Provider properties warn but still work

    Code
      provider@model
    Condition
      Warning:
      `Provider@model` was deprecated in ellmer 0.5.0.
      i Use the `Model` object instead, e.g. `chat$get_model_object()`.
    Output
      [1] "m"
    Code
      provider@params
    Condition
      Warning:
      `Provider@params` was deprecated in ellmer 0.5.0.
      i Use the `Model` object instead, e.g. `chat$get_model_object()`.
    Output
      list()
    Code
      provider@extra_args
    Condition
      Warning:
      `Provider@extra_args` was deprecated in ellmer 0.5.0.
      i Use the `Model` object instead, e.g. `chat$get_model_object()`.
    Output
      list()
    Code
      provider <- Provider(name = "test", base_url = "https://example.com", model = "x")
    Condition
      Warning:
      `Provider(model)` was deprecated in ellmer 0.5.0.
      i Use the `Model` object instead.
    Code
      provider@model
    Condition
      Warning:
      `Provider@model` was deprecated in ellmer 0.5.0.
      i Use the `Model` object instead, e.g. `chat$get_model_object()`.
    Output
      [1] "x"

# Provider print and str omit deprecated properties

    Code
      print(test_provider())
    Output
      <ellmer::Provider>
       @ name         : chr ""
       @ base_url     : chr ""
       @ extra_headers: chr(0) 
       @ credentials  :function ()  
    Code
      str(test_provider())
    Output
      <ellmer::Provider>
       @ name         : chr ""
       @ base_url     : chr ""
       @ extra_headers: chr(0) 
       @ credentials  :function ()  

