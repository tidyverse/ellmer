# Transform litellm model_prices_and_context_window.json into ellmer's prices format.
#
# Input:  litellm JSON object keyed by "provider/model" strings
# Output: array of {provider, model, variant, input?, output?, cached_input?} objects
#         with costs in dollars per million tokens

def provider_map:
  {
    "openai":                        "OpenAI",
    "anthropic":                     "Anthropic",
    "gemini":                        "Google/Gemini",
    "vertex_ai-language-models":     "Google/Vertex",
    "openrouter":                    "OpenRouter",
    "azure":                         "Azure/OpenAI",
    "bedrock":                       "AWS/Bedrock",
    "mistral":                       "Mistral"
  };

# Scale per-token cost to per-million-token cost, rounded to 6 decimal places
def scale: (. * 1e6 * 1e6 | round) / 1e6;

# Conditionally add a field; no-op if value is null
def maybe($k; $v): if $v != null then {($k): ($v | scale)} else {} end;

[
  to_entries[]
  | select(.key != "sample_spec")
  | select(.value.litellm_provider != null)
  | select(provider_map[.value.litellm_provider] != null)
  | .key as $k
  | .value as $e
  | provider_map[$e.litellm_provider] as $p
  | ($k | ltrimstr($e.litellm_provider + "/")) as $m

  | (
      # baseline
      (
        { provider: $p, model: $m, variant: "" }
        + maybe("input";        $e.input_cost_per_token)
        + maybe("output";       $e.output_cost_per_token)
        + maybe("cached_input"; $e.cache_read_input_token_cost)
        | select((.input // 0) > 0 or (.output // 0) > 0)
      ),

      # above_128k_tokens
      (if ($e.input_cost_per_token_above_128k_tokens  != null or
           $e.output_cost_per_token_above_128k_tokens != null) then
        { provider: $p, model: $m, variant: "above_128k_tokens" }
        + maybe("input";  $e.input_cost_per_token_above_128k_tokens)
        + maybe("output"; $e.output_cost_per_token_above_128k_tokens)
      else empty end),

      # above_200k_tokens
      (if ($e.input_cost_per_token_above_200k_tokens  != null or
           $e.output_cost_per_token_above_200k_tokens != null) then
        { provider: $p, model: $m, variant: "above_200k_tokens" }
        + maybe("input";        $e.input_cost_per_token_above_200k_tokens)
        + maybe("output";       $e.output_cost_per_token_above_200k_tokens)
        + maybe("cached_input"; $e.cache_read_input_token_cost_above_200k)
      else empty end),

      # batches
      (if ($e.input_cost_per_token_batches  != null or
           $e.output_cost_per_token_batches != null) then
        { provider: $p, model: $m, variant: "batches" }
        + maybe("input";  $e.input_cost_per_token_batches)
        + maybe("output"; $e.output_cost_per_token_batches)
      else empty end),

      # cache_hit (OpenRouter)
      (if $e.input_cost_per_token_cache_hit != null then
        { provider: $p, model: $m, variant: "cache_hit" }
        + maybe("input"; $e.input_cost_per_token_cache_hit)
      else empty end),

      # flex
      (if ($e.input_cost_per_token_flex  != null or
           $e.output_cost_per_token_flex != null) then
        { provider: $p, model: $m, variant: "flex" }
        + maybe("input";        $e.input_cost_per_token_flex)
        + maybe("output";       $e.output_cost_per_token_flex)
        + maybe("cached_input"; $e.cache_read_input_token_cost_flex)
      else empty end),

      # priority
      (if ($e.input_cost_per_token_priority  != null or
           $e.output_cost_per_token_priority != null) then
        { provider: $p, model: $m, variant: "priority" }
        + maybe("input";        $e.input_cost_per_token_priority)
        + maybe("output";       $e.output_cost_per_token_priority)
        + maybe("cached_input"; $e.cache_read_input_token_cost_priority)
      else empty end)
    )
]
| sort_by(.provider, .model, .variant)
