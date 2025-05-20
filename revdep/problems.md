# GitAI

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/GitAI
* Date/Publication: 2025-02-20 18:40:16 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "GitAI")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
      > # * https://testthat.r-lib.org/articles/special-files.html
    ...
       4.   └─GitAI (local) `<fn>`(model = "gpt-4o-mini", seed = NULL, echo = "none")
       5.     └─GitAI:::mock_chat_method(...) at tests/testthat/setup.R:46:3
       6.       ├─rlang::exec(provider_class, !!!provider_args) at tests/testthat/setup.R:24:3
       7.       └─ellmer (local) `<S7_class>`(...)
       8.         ├─S7::new_object(...)
       9.         └─ellmer::Provider(...)
      
      [ FAIL 4 | WARN 5 | SKIP 7 | PASS 35 ]
      Error: Test failures
      Execution halted
    ```

