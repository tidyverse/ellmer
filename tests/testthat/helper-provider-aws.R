test_aws_bedrock_provider <- function(
  cache_point = "5m",
  model = "anthropic.claude-3-5-haiku-20241022-v1:0"
) {
  cache_point <- as_bedrock_cache_point(cache_point, model)

  ProviderAWSBedrock(
    name = "ProviderAWSBedrock",
    base_url = "https://bedrock-runtime.us-east-1.amazonaws.com",
    model = model,
    profile = NULL,
    region = "us-east-1",
    creds_cache = list(),
    cache_point = cache_point,
    params = list(),
    extra_args = list(),
    extra_headers = character()
  )
}

local_mocked_aws_credentials <- function(
  region = "us-east-1",
  frame = caller_env()
) {
  local_mocked_bindings(
    locate_aws_credentials = function(profile) {
      list(
        access_key_id = "access-key-id",
        secret_access_key = "secret-access-key",
        region = region,
        expiration = Sys.time() + 3600
      )
    },
    .env = frame
  )

  # Credentials are cached by profile, so a previous test's region would
  # otherwise leak into this one.
  aws_creds_cache(NULL)$clear()
  withr::defer(aws_creds_cache(NULL)$clear(), envir = frame)
}
