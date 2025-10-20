# useful message if no tokens

    Code
      token_usage()
    Message
      x No recorded usage in this session

# can retrieve and log tokens

    Code
      token_usage()
    Output
            provider model variant input output cached_input price
      1 testprovider  test             1      1            1    NA

# token_usage() shows price if available

    Code
      token_usage()
    Output
        provider  model variant   input output cached_input price
      1   OpenAI gpt-4o         1500000  2e+05            0 $5.75

