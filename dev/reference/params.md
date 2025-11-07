# Standard model parameters

This helper function makes it easier to create a list of parameters used
across many models. The parameter names are automatically standardised
and included in the correctly place in the API call.

Note that parameters that are not supported by a given provider will
generate a warning, not an error. This allows you to use the same set of
parameters across multiple providers.

## Usage

``` r
params(
  temperature = NULL,
  top_p = NULL,
  top_k = NULL,
  frequency_penalty = NULL,
  presence_penalty = NULL,
  seed = NULL,
  max_tokens = NULL,
  log_probs = NULL,
  stop_sequences = NULL,
  reasoning_effort = NULL,
  reasoning_tokens = NULL,
  ...
)
```

## Arguments

- temperature:

  Temperature of the sampling distribution.

- top_p:

  The cumulative probability for token selection.

- top_k:

  The number of highest probability vocabulary tokens to keep.

- frequency_penalty:

  Frequency penalty for generated tokens.

- presence_penalty:

  Presence penalty for generated tokens.

- seed:

  Seed for random number generator.

- max_tokens:

  Maximum number of tokens to generate.

- log_probs:

  Include the log probabilities in the output?

- stop_sequences:

  A character vector of tokens to stop generation on.

- reasoning_effort, reasoning_tokens:

  How much effort to spend thinking? `ressoning_effort` is a string,
  like "low", "medium", "high". `reasoning_tokens` is an integer, giving
  a maximum token budget. Each provider only takes one of these two
  parameters.

- ...:

  Additional named parameters to send to the provider.
