# A chatbot provider

A Provider captures the details of one chatbot service/API. This
captures how the API works, not the details of the underlying large
language model. Different providers might offer the same (open source)
model behind a different API.

## Usage

``` r
Provider(
  name = stop("Required"),
  model = stop("Required"),
  base_url = stop("Required"),
  params = list(),
  extra_args = list(),
  extra_headers = character(0),
  credentials = function() NULL
)
```

## Arguments

- name:

  Name of the provider.

- model:

  Name of the model.

- base_url:

  The base URL for the API.

- params:

  A list of standard parameters created by
  [`params()`](https://ellmer.tidyverse.org/dev/reference/params.md).

- extra_args:

  Arbitrary extra arguments to be included in the request body.

- extra_headers:

  Arbitrary extra headers to be added to the request.

- credentials:

  A zero-argument function that returns the credentials to use for
  authentication. Can either return a string, representing an API key,
  or a named list of headers.

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
  model = "my_model",
  base_url = "https://cool-models.com"
)
#> <ellmer::Provider>
#>  @ name         : chr "CoolModels"
#>  @ model        : chr "my_model"
#>  @ base_url     : chr "https://cool-models.com"
#>  @ params       : list()
#>  @ extra_args   : list()
#>  @ extra_headers: chr(0) 
#>  @ credentials  : function ()  
```
