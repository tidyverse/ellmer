# chat_snowflake_cortex() is deprecated

    Code
      chat_cortex_analyst(model_spec = "test")
    Condition
      Warning:
      `char_cortex_analyst()` was deprecated in ellmer 0.3.0.
      i Please use `chat_snowflake()` instead.
    Output
      <Chat Snowflake/CortexAnalyst/ turns=0 tokens=0/0>

# Cortex turn formatting

    Code
      cat(turn@text)
    Output
      This semantic data model...
      
      ```sql
      SELECT SUM(revenue) FROM key_business_metrics
      ```
      
      #### Suggestions
      
      - What is the total quantity sold for each product last quarter?
      - What is the average discount percentage for orders from the United States?
      - What is the average price of products in the 'electronics' category?

---

    Code
      cat(format(turn))
    Output
      This semantic data model...
      SQL: `SELECT SUM(revenue) FROM key_business_metrics`
      Suggestions:
      * What is the total quantity sold for each product last quarter?
      * What is the average discount percentage for orders from the United States?
      * What is the average price of products in the 'electronics' category?

# Cortex API requests are generated correctly

    Code
      str(request_summary(req))
    Output
      List of 3
       $ url    : chr "https://testorg-test_account.snowflakecomputing.com/api/v2/cortex/analyst/message"
       $ headers:List of 2
        ..$ Authorization                       : chr "Bearer obfuscated"
        ..$ X-Snowflake-Authorization-Token-Type: chr "OAUTH"
       $ body   :List of 3
        ..$ messages           :List of 1
        .. ..$ :List of 2
        .. .. ..$ role   : chr "user"
        .. .. ..$ content:List of 1
        .. .. .. ..$ :List of 2
        .. .. .. .. ..$ type: chr "text"
        .. .. .. .. ..$ text: chr "Tell me about my data."
        ..$ stream             : logi FALSE
        ..$ semantic_model_file: chr "@my_db.my_schema.my_stage/model.yaml"

