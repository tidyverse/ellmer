# chattr

<details>

* Version: 0.3.0
* GitHub: https://github.com/mlverse/chattr
* Source code: https://github.com/cran/chattr
* Date/Publication: 2025-05-28 18:30:02 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::cloud_details(, "chattr")` for more info

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
      > # * https://r-pkgs.org/tests.html
      > # * https://testthat.r-lib.org/reference/test_package.html#special-files
    ...
       22.     └─cli::cli_abort(...)
       23.       └─rlang::abort(...)
      
      [ FAIL 1 | WARN 1 | SKIP 26 | PASS 38 ]
      Deleting unused snapshots:
      • app-server/001.json
      • app-server/002.json
      • app-server/003.json
      Error: Test failures
      Execution halted
    ```

