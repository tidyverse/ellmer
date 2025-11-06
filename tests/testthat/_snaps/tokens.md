# useful message if no tokens

    Code
      token_usage()
    Message
      x No recorded usage in this session

# can retrieve and log tokens

    Code
      token_usage()
    Output
            provider model input output cached_input price
      1 testprovider  test     1      1            1 $0.00

# token_usage() shows price if available

    Code
      token_usage()
    Output
        provider  model   input output cached_input price
      1   OpenAI gpt-4o 1500000  2e+05            0 $5.75

# dollars looks good, including in data.frames

    Code
      price
    Output
      [1] $1.23
    Code
      data.frame(price)
    Output
        price
      1 $1.23

