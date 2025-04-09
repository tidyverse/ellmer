# invalid inputs give useful errors

    Code
      chat$chat(question = "Are unicorns real?")
    Condition
      Error in `chat$chat()`:
      ! `...` must be unnamed.
    Code
      chat$chat(TRUE)
    Condition
      Error in `FUN()`:
      ! `...` must be made up strings or <content> objects, not `TRUE`.

# turn contents can be converted to text, markdown and HTML

    Code
      cat(contents_text(turn))
    Output
      User input.
      
      ```sql
      SELECT * FROM mtcars
      ```
      
      #### Suggestions
      
      - What is the total quantity sold for each product last quarter?
      - What is the average discount percentage for orders from the United States?
      - What is the average price of products in the 'electronics' category?

---

    Code
      cat(contents_markdown(turn))
    Output
      User input.
      
      ![](data:image/png;base64,abcd123)
      
      ![](https://example.com/image.jpg)
      
      ```json
      {
        "a": [1, 2],
        "b": "apple"
      }
      ```
      
      
      
      
      ```sql
      SELECT * FROM mtcars
      ```
      
      
      
      #### Suggestions
      
      - What is the total quantity sold for each product last quarter?
      - What is the average discount percentage for orders from the United States?
      - What is the average price of products in the 'electronics' category?

---

    Code
      cat(contents_markdown(chat))
    Output
      ## User
      
      User input.
      
      ![](data:image/png;base64,abcd123)
      
      ![](https://example.com/image.jpg)
      
      ```json
      {
        "a": [1, 2],
        "b": "apple"
      }
      ```
      
      
      
      
      ```sql
      SELECT * FROM mtcars
      ```
      
      
      
      #### Suggestions
      
      - What is the total quantity sold for each product last quarter?
      - What is the average discount percentage for orders from the United States?
      - What is the average price of products in the 'electronics' category?
      
      ## Assistant
      
      Here's your answer.

---

    Code
      cat(contents_html(turn))
    Output
      <p>User input.</p>
      
      <img src="data:image/png;base64,abcd123">
      <img src="https://example.com/image.jpg">
      <pre><code>{
        "a": [1, 2],
        "b": "apple"
      }</code></pre>
      
      <pre><code>SELECT * FROM mtcars</code></pre>

# thinking has useful representations

    Code
      cat(contents_html(ct))
    Output
      <details><summary>Thinking</summary>
      <p>A <strong>thought</strong>.</p>
      </details>

# ContentToolResult@error requires a string or an error condition

    Code
      ContentToolResult("id", error = TRUE)
    Condition
      Error:
      ! <ellmer::ContentToolResult> object properties are invalid:
      - @error must be <NULL>, <character>, or S3<condition>, not <logical>
    Code
      ContentToolResult("id", error = c("one", "two"))
    Condition
      Error:
      ! <ellmer::ContentToolResult> object properties are invalid:
      - @error must be a single string or a condition object, not a character vector.

