# querychat (0.3.0)

* GitHub: <https://github.com/posit-dev/querychat>
* Email: <mailto:garrick@posit.co>
* GitHub mirror: <https://github.com/cran/querychat>

Run `revdepcheck::cloud_details(, "querychat")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
         'test-DataFrameSource.R:24:3', 'test-DataFrameSource.R:153:5',
         'test-DataSource.R:2:3', 'test-DataSource.R:388:3', 'test-QueryChat.R:81:3',
         'test-QueryChat.R:656:1', 'test-QueryChat.R:829:3',
         'test-querychat_tools.R:1:1', 'test-querychat_tools.R:14:1',
         'test-querychat_tools.R:18:1', 'test-TblSqlSource.R:12:3'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test-QueryChat.R:637:3'): QueryChat$generate_greeting() generates a greeting using the LLM client ──
       Error in `initialize(...)`: `model` is required.
       Backtrace:
           ▆
        1. └─querychat:::mock_ellmer_chat_client(...) at test-QueryChat.R:637:3
        2.   └─MockChat$new(ellmer::Provider("test", "test", "test")) at ./helper-fixtures.R:171:3
        3.     └─ellmer (local) initialize(...)
        4.       └─cli::cli_abort("{.arg model} is required.")
        5.         └─rlang::abort(...)
       ── Failure ('test-querychat_module.R:14:3'): Shiny app example loads without errors ──
       Expected `{ ... }` not to throw any errors.
       Actually got a <rlang_error> with message:
         `model` is required.
       
       [ FAIL 2 | WARN 2 | SKIP 15 | PASS 629 ]
       Error:
       ! Test failures.
       Execution halted
     ```

