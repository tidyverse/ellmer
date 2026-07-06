test_aws_bedrock_provider <- function(
  cache_point = "5m",
  model = "anthropic.claude-3-5-haiku-20241022-v1:0"
) {
  cache_policy <- cache_point
  cache_point <- as_bedrock_cache_point(cache_policy, model)

  ProviderAWSBedrock(
    name = "ProviderAWSBedrock",
    base_url = "https://bedrock-runtime.us-east-1.amazonaws.com",
    model = model,
    profile = NULL,
    region = "us-east-1",
    cache = list(),
    cache_policy = cache_policy,
    cache_point = cache_point,
    extra_headers = character()
  )
}
