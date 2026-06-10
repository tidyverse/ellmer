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

