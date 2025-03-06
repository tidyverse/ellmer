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
      req$url
    Output
      [1] "https://testorg-test_account.snowflakecomputing.com/api/v2/cortex/analyst/message"

---

    Code
      req$headers
    Output
      <httr2_headers>
      Authorization: <REDACTED>
      X-Snowflake-Authorization-Token-Type: <REDACTED>

---

    Code
      print_json(req$body$data)
    Output
      {
        "messages": [
          {
            "role": "user",
            "content": [
              {
                "type": "text",
                "text": "Tell me about my data."
              }
            ]
          }
        ],
        "stream": false,
        "semantic_model_file": "@my_db.my_schema.my_stage/model.yaml"
      }

