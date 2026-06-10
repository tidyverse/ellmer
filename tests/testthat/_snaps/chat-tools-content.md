# expanding generates useful JSON

    Code
      print_json(as_json(provider, UserTurn(expanded_simple)))
    Output
      [
        {
          "type": "function_call_output",
          "call_id": "123",
          "output": "See <tool-content call-id=\"123\"> below."
        },
        {
          "role": "user",
          "content": [
            {
              "type": "input_text",
              "text": "<tool-content call-id=\"123\">"
            }
          ]
        },
        {
          "role": "user",
          "content": [
            {
              "type": "input_image",
              "image_url": "data:image/png;base64,abc"
            }
          ]
        },
        {
          "role": "user",
          "content": [
            {
              "type": "input_text",
              "text": "<\/tool-content>"
            }
          ]
        }
      ]

---

    Code
      print_json(as_json(provider, UserTurn(expanded_list)))
    Output
      [
        {
          "type": "function_call_output",
          "call_id": "123",
          "output": "See <tool-contents call-id=\"123\"> below."
        },
        {
          "role": "user",
          "content": [
            {
              "type": "input_text",
              "text": "<tool-contents call-id=\"123\">"
            }
          ]
        },
        {
          "role": "user",
          "content": [
            {
              "type": "input_text",
              "text": "<tool-content>"
            }
          ]
        },
        {
          "role": "user",
          "content": [
            {
              "type": "input_image",
              "image_url": "data:image/png;base64,abc"
            }
          ]
        },
        {
          "role": "user",
          "content": [
            {
              "type": "input_text",
              "text": "<\/tool-content>"
            }
          ]
        },
        {
          "role": "user",
          "content": [
            {
              "type": "input_text",
              "text": "<tool-content>"
            }
          ]
        },
        {
          "role": "user",
          "content": [
            {
              "type": "input_image",
              "image_url": "data:image/png;base64,abc"
            }
          ]
        },
        {
          "role": "user",
          "content": [
            {
              "type": "input_text",
              "text": "<\/tool-content>"
            }
          ]
        },
        {
          "role": "user",
          "content": [
            {
              "type": "input_text",
              "text": "<\/tool-contents>"
            }
          ]
        }
      ]

# programmatic tool results can't expand rich content

    Code
      turn_contents_expand(turn)
    Condition
      Error:
      ! Tool `my_tool` must return a string or JSON-convertible value when called programmatically.
      i Results of programmatic tool calls are delivered to code running in Claude's sandbox, which can't see other content blocks.

---

    Code
      turn_contents_expand(turn)
    Condition
      Error:
      ! Tool `my_tool` must return a string or JSON-convertible value when called programmatically.
      i Results of programmatic tool calls are delivered to code running in Claude's sandbox, which can't see other content blocks.

