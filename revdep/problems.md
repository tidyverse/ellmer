# constructive (1.3.0)

* GitHub: <https://github.com/cynkra/constructive>
* Email: <mailto:antoine.fabri@gmail.com>
* GitHub mirror: <https://github.com/cran/constructive>

Run `revdepcheck::cloud_details(, "constructive")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       • with_versions(R >= "4.3") is TRUE (3): 'test-s3-POSIXct.R:2:3',
         'test-s3-POSIXlt.R:66:3', 'test-s3-mts.R:11:3'
       • with_versions(ellmer > "0.2.1") is TRUE (1): 'test-s7-elmer_TypeBasic.R:31:3'
       • with_versions(ggplot2 > "3.5.2") is TRUE (2): 'test-s3-ggplot2-Coord.R:19:3',
         'test-s3-ggplot2-Coord.R:66:3'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Failure ('test-s7-elmer_TypeBasic.R:3:3'): ellmer::TypeBasic ────────────────
       Expected `recreated` to equal `expected`.
       Differences:
       actual vs expected
         `ellmer::type_array(items = ellmer::type_object(x = ellmer::type_boolean(), `
       - `    y = ellmer::type_string(), z = ellmer::type_number(), json = ellmer::type_from_schema("[1,2]"), `
       - `    .additional_properties = FALSE))`
       + `    y = ellmer::type_string(), z = ellmer::type_number(), json = ellmer::type_from_schema("[1,2]")))`
       
       Backtrace:
           ▆
        1. └─constructive:::expect_construct(...) at test-s7-elmer_TypeBasic.R:3:3
        2.   └─testthat::expect_equal(recreated, expected)
       
       [ FAIL 1 | WARN 2 | SKIP 135 | PASS 97 ]
       Error:
       ! Test failures.
       Execution halted
     ```

