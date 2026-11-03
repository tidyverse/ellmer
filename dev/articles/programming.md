# Programming with ellmer

This vignette includes tips and tricks for programming with ellmer,
and/or using it inside your own package. It’s currently fairly short but
will grow over time.

The examples use `gpt-5.4-mini`, a cheap OpenAI model. Any provider
works the same way, but the exact responses vary from model to model,
and a couple of the examples rely on behavior that smaller models don’t
reliably show. Those are noted in the code.

``` r

library(ellmer)
```

## Cloning chats

Chat objects are [R6 objects](https://r6.r-lib.org), which means that
they are **mutable**. Most R objects are immutable. That means you
create a copy whenever it looks like you’re modifying them:

``` r

x <- list(a = 1, b = 2)

f <- function() {
  x$a <- 100
}
f()

# The original x is unchanged
str(x)
#> List of 2
#>  $ a: num 1
#>  $ b: num 2
```

Mutable objects don’t work the same way:

``` r

chat <- chat_openai("Be terse", model = "gpt-5.4-mini")

capital <- function(chat, country) {
  chat$chat(interpolate("What's the capital of {{country}}"))
}
capital(chat, "New Zealand")
#> Wellington.
capital(chat, "France")
#> Paris.

chat
#> <Chat OpenAI/gpt-5.4-mini turns=5 input=54 output=13 cost=$0.00>
#> ── system ─────────────────────────────────────────────────────────────
#> Be terse
#> ── user ───────────────────────────────────────────────────────────────
#> What's the capital of New Zealand
#> ── assistant [input=18 output=7 cost=$0.00] ───────────────────────────
#> Wellington.
#> ── user ───────────────────────────────────────────────────────────────
#> What's the capital of France
#> ── assistant [input=36 output=6 cost=$0.00] ───────────────────────────
#> Paris.
```

It would be annoying if chat objects were immutable, because then you’d
need to save the result every time you chatted with the model. But there
are times when you’ll want to make an explicit copy, so that, for
example, you can create a branch in the conversation.

Creating a copy of the object is the job of the `$clone()` method. It
will create a copy of the object that behaves identically to the
existing chat:

``` r

chat <- chat_openai("Be terse", model = "gpt-5.4-mini")

capital <- function(chat, country) {
  chat <- chat$clone()
  chat$chat(interpolate("What's the capital of {{country}}"))
}
capital(chat, "New Zealand")
#> Wellington.
capital(chat, "France")
#> Paris.

chat
#> <Chat OpenAI/gpt-5.4-mini turns=1 input=0 output=0 cost=$0.00>
#> ── system ─────────────────────────────────────────────────────────────
#> Be terse
```

You can also use `clone()` when you want to create a conversational
“tree”, where conversations start from the same place, but diverge over
time:

``` r

chat1 <- chat_openai("Be terse", model = "gpt-5.4-mini")
chat1$chat("My name is Hadley and I'm a data scientist")
#> Nice to meet you, Hadley.
chat2 <- chat1$clone()

chat1$chat("what's my name?")
#> Hadley
chat1
#> <Chat OpenAI/gpt-5.4-mini turns=5 input=67 output=18 cost=$0.00>
#> ── system ─────────────────────────────────────────────────────────────
#> Be terse
#> ── user ───────────────────────────────────────────────────────────────
#> My name is Hadley and I'm a data scientist
#> ── assistant [input=22 output=12 cost=$0.00] ──────────────────────────
#> Nice to meet you, Hadley.
#> ── user ───────────────────────────────────────────────────────────────
#> what's my name?
#> ── assistant [input=45 output=6 cost=$0.00] ───────────────────────────
#> Hadley

chat2$chat("what's my job?")
#> You’re a data scientist.
chat2
#> <Chat OpenAI/gpt-5.4-mini turns=5 input=67 output=22 cost=$0.00>
#> ── system ─────────────────────────────────────────────────────────────
#> Be terse
#> ── user ───────────────────────────────────────────────────────────────
#> My name is Hadley and I'm a data scientist
#> ── assistant [input=22 output=12 cost=$0.00] ──────────────────────────
#> Nice to meet you, Hadley.
#> ── user ───────────────────────────────────────────────────────────────
#> what's my job?
#> ── assistant [input=45 output=10 cost=$0.00] ──────────────────────────
#> You’re a data scientist.
```

(This is the technique that
[`parallel_chat()`](https://ellmer.tidyverse.org/dev/reference/parallel_chat.md)
uses internally.)

## Resetting an object

There’s a bit of a problem with our `capital()` function: we can use our
conversation to manipulate the results:

``` r

# Smaller models tend to refuse to pretend, so this needs a reasonably capable model
chat <- chat_openai("Be terse", model = "gpt-5.4-mini")
chat$chat("Pretend that the capital of New Zealand is Kiwicity")
#> Okay — I’ll treat **Kiwicity** as the capital of New Zealand for this 
#> conversation.
capital(chat, "New Zealand")
#> Kiwicity
```

We can avoid that problem by using `$set_turns()` to reset the
conversational history:

``` r

chat <- chat_openai("Be terse", model = "gpt-5.4-mini")
chat$chat("Pretend that the capital of New Zealand is Kiwicity")
#> Sure — I can do that in this conversation.
#> 
#> New Zealand’s capital is Kiwicity.

capital <- function(chat, country) {
  chat <- chat$clone()$set_turns(list())
  chat$chat(interpolate("What's the capital of {{country}}"))
}
capital(chat, "New Zealand")
#> Wellington.
```

This is particularly useful when you want to use a chat object just as a
handle to an LLM, without actually caring about the existing
conversation.

## Streaming vs batch results

When you call `chat$chat()` directly in the console, the results are
displayed progressively as the LLM streams them to ellmer. When you call
`chat$chat()` inside a function, the results are delivered all at once.
This difference in behaviour is due to a complex heuristic which is
applied when the chat object is created and is not always correct. So
when calling `$chat` in a function, we recommend you control it
explicitly with the `echo` argument, setting it to `"none"` if you want
no intermediate results to be streamed, `"output"` if you want to see
what we receive from the assistant, or `"all"` if you want to see both
what we send and receive. You likely want `echo = "none"` in most cases:

``` r

capital <- function(chat, country) {
  chat <- chat$clone()$set_turns(list())
  chat$chat(interpolate("What's the capital of {{country}}"), echo = "none")
}
capital(chat, "France")
#> Paris.
```

Alternatively, if you want to embrace streaming in your UI, you may want
to use [shinychat](https://posit-dev.github.io/shinychat/) (for Shiny)
or [streamy](https://simonpcouch.github.io/streamy/) (for
Positron/RStudio).

## Turns and content

Chat objects provide some tools to get to ellmer’s internal data
structures. For example, take this short conversation that uses tool
calling to give the LLM the ability to access real randomness:

``` r

set.seed(1014) # make it reproducible

# Smaller models often make the two tool calls one at a time rather than together
chat <- chat_openai("Be terse", model = "gpt-5.4-mini")
chat$register_tool(tool(function() sample(6, 1), "Roll a die"))
chat$chat("Roll two dice and tell me the total")
#> The total is **9**.

chat
#> <Chat OpenAI/gpt-5.4-mini turns=5 input=167 output=56 cost=$0.00>
#> ── system ─────────────────────────────────────────────────────────────
#> Be terse
#> ── user ───────────────────────────────────────────────────────────────
#> Roll two dice and tell me the total
#> ── assistant [input=49 output=45 cost=$0.00] ──────────────────────────
#> [tool request (fc_06eb20f0b078d03d016ae7a5b7570487d2818db3f0502cf562)]: tool_001()
#> [tool request (fc_06eb20f0b078d03d016ae7a5b7571487d2a370515a0fa3375d)]: tool_001()
#> ── user ───────────────────────────────────────────────────────────────
#> [tool result  (fc_06eb20f0b078d03d016ae7a5b7570487d2818db3f0502cf562)]: 5
#> [tool result  (fc_06eb20f0b078d03d016ae7a5b7571487d2a370515a0fa3375d)]: 4
#> ── assistant [input=118 output=11 cost=$0.00] ─────────────────────────
#> The total is **9**.
```

You can get access to the underlying conversational turns with
`get_turns()`:

``` r

turns <- chat$get_turns()
turns
#> [[1]]
#> <Turn: user>
#> Roll two dice and tell me the total
#> 
#> [[2]]
#> <Turn: assistant>
#> [tool request (fc_06eb20f0b078d03d016ae7a5b7570487d2818db3f0502cf562)]: tool_001()
#> [tool request (fc_06eb20f0b078d03d016ae7a5b7571487d2a370515a0fa3375d)]: tool_001()
#> 
#> [[3]]
#> <Turn: user>
#> [tool result  (fc_06eb20f0b078d03d016ae7a5b7570487d2818db3f0502cf562)]: 5
#> [tool result  (fc_06eb20f0b078d03d016ae7a5b7571487d2a370515a0fa3375d)]: 4
#> 
#> [[4]]
#> <Turn: assistant>
#> The total is **9**.
```

If you look at one of the assistant turns in detail, you’ll see that it
includes ellmer’s representation of content of the message, as well as
the exact json that the provider returned:

``` r

str(turns[[2]])
#> <ellmer::AssistantTurn>
#>  @ contents     :List of 2
#>  .. $ : <ellmer::ContentToolRequest>
#>  ..  ..@ id       : chr "fc_06eb20f0b078d03d016ae7a5b7570487d2818db3f0502cf562"
#>  ..  ..@ name     : chr "tool_001"
#>  ..  ..@ arguments: Named list()
#>  ..  ..@ tool     : <ellmer::ToolDef> function ()  
#>  .. .. .. @ name       : chr "tool_001"
#>  .. .. .. @ description: chr "Roll a die"
#>  .. .. .. @ arguments  : <ellmer::TypeObject>
#>  .. .. .. .. @ description          : NULL
#>  .. .. .. .. @ required             : logi TRUE
#>  .. .. .. .. @ properties           : list()
#>  .. .. .. .. @ additional_properties: logi FALSE
#>  .. .. .. @ convert    : logi TRUE
#>  .. .. .. @ annotations: list()
#>  ..  ..@ extra    : list()
#>  .. $ : <ellmer::ContentToolRequest>
#>  ..  ..@ id       : chr "fc_06eb20f0b078d03d016ae7a5b7571487d2a370515a0fa3375d"
#>  ..  ..@ name     : chr "tool_001"
#>  ..  ..@ arguments: Named list()
#>  ..  ..@ tool     : <ellmer::ToolDef> function ()  
#>  .. .. .. @ name       : chr "tool_001"
#>  .. .. .. @ description: chr "Roll a die"
#>  .. .. .. @ arguments  : <ellmer::TypeObject>
#>  .. .. .. .. @ description          : NULL
#>  .. .. .. .. @ required             : logi TRUE
#>  .. .. .. .. @ properties           : list()
#>  .. .. .. .. @ additional_properties: logi FALSE
#>  .. .. .. @ convert    : logi TRUE
#>  .. .. .. @ annotations: list()
#>  ..  ..@ extra    : list()
#>  @ text         : chr ""
#>  @ role         : chr "assistant"
#>  @ json         :List of 37
#>  .. $ id                    : chr "resp_06eb20f0b078d03d016ae7a5b669ec87d2a7fa617773e99704"
#>  .. $ object                : chr "response"
#>  .. $ created_at            : int 1793566134
#>  .. $ status                : chr "completed"
#>  .. $ access_programs       : NULL
#>  .. $ background            : logi FALSE
#>  .. $ billing               :List of 1
#>  ..  ..$ payer: chr "developer"
#>  .. $ completed_at          : int 1793566135
#>  .. $ error                 : NULL
#>  .. $ frequency_penalty     : num 0
#>  .. $ incomplete_details    : NULL
#>  .. $ instructions          : NULL
#>  .. $ max_output_tokens     : NULL
#>  .. $ max_tool_calls        : NULL
#>  .. $ model                 : chr "gpt-5.4-mini-2026-03-17"
#>  .. $ moderation            : NULL
#>  .. $ output                :List of 2
#>  ..  ..$ :List of 6
#>  ..  .. ..$ id       : chr "fc_06eb20f0b078d03d016ae7a5b7570487d2818db3f0502cf562"
#>  ..  .. ..$ type     : chr "function_call"
#>  ..  .. ..$ status   : chr "completed"
#>  ..  .. ..$ arguments: chr "{}"
#>  ..  .. ..$ call_id  : chr "call_QiHq0r6KuAr4T8h03QKwh1QD"
#>  ..  .. ..$ name     : chr "tool_001"
#>  ..  ..$ :List of 6
#>  ..  .. ..$ id       : chr "fc_06eb20f0b078d03d016ae7a5b7571487d2a370515a0fa3375d"
#>  ..  .. ..$ type     : chr "function_call"
#>  ..  .. ..$ status   : chr "completed"
#>  ..  .. ..$ arguments: chr "{}"
#>  ..  .. ..$ call_id  : chr "call_XthPRNp1a7nUx3D3WSELcjR8"
#>  ..  .. ..$ name     : chr "tool_001"
#>  .. $ parallel_tool_calls   : logi TRUE
#>  .. $ presence_penalty      : num 0
#>  .. $ previous_response_id  : NULL
#>  .. $ prompt_cache_key      : NULL
#>  .. $ prompt_cache_retention: chr "24h"
#>  .. $ reasoning             :List of 4
#>  ..  ..$ context: chr "current_turn"
#>  ..  ..$ effort : chr "none"
#>  ..  ..$ mode   : chr "standard"
#>  ..  ..$ summary: NULL
#>  .. $ safety_identifier     : NULL
#>  .. $ service_tier          : chr "default"
#>  .. $ store                 : logi FALSE
#>  .. $ temperature           : num 1
#>  .. $ text                  :List of 2
#>  ..  ..$ format   :List of 1
#>  ..  .. ..$ type: chr "text"
#>  ..  ..$ verbosity: chr "medium"
#>  .. $ tool_choice           : chr "auto"
#>  .. $ tool_usage            :List of 2
#>  ..  ..$ image_gen :List of 5
#>  ..  .. ..$ input_tokens         : int 0
#>  ..  .. ..$ input_tokens_details :List of 2
#>  ..  .. .. ..$ image_tokens: int 0
#>  ..  .. .. ..$ text_tokens : int 0
#>  ..  .. ..$ output_tokens        : int 0
#>  ..  .. ..$ output_tokens_details:List of 2
#>  ..  .. .. ..$ image_tokens: int 0
#>  ..  .. .. ..$ text_tokens : int 0
#>  ..  .. ..$ total_tokens         : int 0
#>  ..  ..$ web_search:List of 1
#>  ..  .. ..$ num_requests: int 0
#>  .. $ tools                 :List of 1
#>  ..  ..$ :List of 6
#>  ..  .. ..$ type         : chr "function"
#>  ..  .. ..$ description  : chr "Roll a die"
#>  ..  .. ..$ name         : chr "tool_001"
#>  ..  .. ..$ output_schema: NULL
#>  ..  .. ..$ parameters   :List of 5
#>  ..  .. .. ..$ type                : chr "object"
#>  ..  .. .. ..$ description         : chr ""
#>  ..  .. .. ..$ properties          : Named list()
#>  ..  .. .. ..$ required            : list()
#>  ..  .. .. ..$ additionalProperties: logi FALSE
#>  ..  .. ..$ strict       : logi TRUE
#>  .. $ top_logprobs          : int 0
#>  .. $ top_p                 : num 0.98
#>  .. $ truncation            : chr "disabled"
#>  .. $ usage                 :List of 5
#>  ..  ..$ input_tokens         : int 49
#>  ..  ..$ input_tokens_details :List of 2
#>  ..  .. ..$ cache_write_tokens: int 0
#>  ..  .. ..$ cached_tokens     : int 0
#>  ..  ..$ output_tokens        : int 45
#>  ..  ..$ output_tokens_details:List of 1
#>  ..  .. ..$ reasoning_tokens: int 0
#>  ..  ..$ total_tokens         : int 94
#>  .. $ user                  : NULL
#>  .. $ metadata              : Named list()
#>  @ tokens       : Named int [1:3] 49 45 0
#>  .. - attr(*, "names")= chr [1:3] "input" "output" "cached_input"
#>  @ cost         : 'ellmer_dollars' num $0.00
#>  @ duration     : num NA
#>  @ finish_reason: chr "tool_use"
```

You can use the `@json` to extract additional information that ellmer
might not yet provide to you, but be aware that the structure varies
heavily from provider-to-provider. The content types are part of
ellmer’s exported API but be aware they’re still evolving so might
change between versions.
