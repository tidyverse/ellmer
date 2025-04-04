# useful message if no tokens

    Code
      token_usage()
    Message
      x No recorded usage in this session

# can retrieve and log tokens

    Code
      token_usage()
    Output
            provider model input output price
      1 testprovider  test    10     60    NA

# token_usage() shows price if available

    Code
      token_usage()
    Output
            provider model    input output price
      1 testprovider  test 12300000 678000 $1.24

