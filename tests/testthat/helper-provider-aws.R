local_mock_aws_creds <- function(env = caller_env()) {
  local_mocked_bindings(
    locate_aws_credentials = function(profile) {
      list(
        access_key_id = "test-key",
        secret_access_key = "test-secret",
        session_token = "test-token",
        region = "us-east-1",
        expiration = Sys.time() + 3600
      )
    },
    .env = env
  )
}
