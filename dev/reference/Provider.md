# A chatbot provider

A Provider captures the details of one chatbot service/API. This
captures how the API works, not the details of the underlying large
language model. Different providers might offer the same (open source)
model behind a different API.

## Usage

``` r
Provider(
  name = stop("Required"),
  base_url = stop("Required"),
  extra_headers = character(0),
  credentials = function() NULL,
  model = NULL,
  params = NULL,
  extra_args = NULL
)
```

## Arguments

- name:

  Name of the provider.

- base_url:

  The base URL for the API.

- extra_headers:

  Arbitrary extra headers to be added to the request.

- credentials:

  A zero-argument function that returns the credentials to use for
  authentication. Can either return a string, representing an API key,
  or a named list of headers.

- model, params, extra_args:

  **\[deprecated\]** These now live on the
  [Model](https://ellmer.tidyverse.org/dev/reference/Model.md) object;
  use `chat$get_model_object()` instead.

## Value

An S7 Provider object.

## Details

To add support for a new backend, you will need to subclass `Provider`
(adding any additional fields that your provider needs) and then
implement the various generics that control the behavior of each
provider.

## Examples

``` r
Provider(
  name = "CoolModels",
  base_url = "https://cool-models.com"
)
#> <ellmer::Provider>
#>  @ name         : chr "CoolModels"
#>  @ base_url     : chr "https://cool-models.com"
#>  @ extra_headers: chr(0) 
#>  @ credentials  :function ()  
```
