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
      list(url = req$url, headers = req$headers, body = req$body$data)
    Output
      $url
      [1] "https://testorg-test_account.snowflakecomputing.com/api/v2/cortex/analyst/message"
      
      $headers
      $headers$Authorization
      [1] "Bearer obfuscated"
      
      $headers$`X-Snowflake-Authorization-Token-Type`
      [1] "OAUTH"
      
      attr(,"redact")
      [1] "Authorization"
      
      $body
      $body$messages
      $body$messages[[1]]
      $body$messages[[1]]$role
      [1] "user"
      
      $body$messages[[1]]$content
      $body$messages[[1]]$content[[1]]
      $body$messages[[1]]$content[[1]]$type
      [1] "text"
      
      $body$messages[[1]]$content[[1]]$text
      [1] "Tell me about my data."
      
      
      
      
      
      $body$stream
      [1] FALSE
      
      $body$semantic_model_file
      [1] "@my_db.my_schema.my_stage/model.yaml"
      
      

---

    Code
      req$body$data
    Output
      $messages
      $messages[[1]]
      $messages[[1]]$role
      [1] "user"
      
      $messages[[1]]$content
      $messages[[1]]$content[[1]]
      $messages[[1]]$content[[1]]$type
      [1] "text"
      
      $messages[[1]]$content[[1]]$text
      [1] "Tell me about my data."
      
      
      
      
      
      $stream
      [1] FALSE
      
      $semantic_model_file
      [1] "@my_db.my_schema.my_stage/model.yaml"
      

