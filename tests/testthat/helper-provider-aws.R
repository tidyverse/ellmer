test_aws_bedrock_provider <- function(cache_point = "5m") {
  ProviderAWSBedrock(
    name = "ProviderAWSBedrock",
    base_url = "https://bedrock-runtime.us-east-1.amazonaws.com",
    model = "anthropic.claude-3-5-haiku-20241022-v1:0",
    profile = NULL,
    region = "us-east-1",
    cache = list(),
    cache_point = cache_point,
    params = list(),
    extra_args = list(),
    extra_headers = character()
  )
}
