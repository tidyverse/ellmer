# ellmercodex (0.2.0)

* GitHub: <https://github.com/simxnherrera/ellmercodex>
* Email: <mailto:simxnherrera@gmail.com>
* GitHub mirror: <https://github.com/cran/ellmercodex>

Run `revdepcheck::cloud_details(, "ellmercodex")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
        10.           └─coro (local) .x()
        11.             └─base::evalq(...)
        12.               └─base::evalq(...)
        13.                 └─base::evalq(...)
        14.                   └─base::evalq(...)
        15.                     ├─coro::is_exhausted(elt <- iterator())
        16.                     └─coro (local) iterator()
        17.                       └─base::evalq(...)
        18.                         └─base::evalq(...)
        19.                           ├─base::evalq(...)
        20.                           │ └─base::evalq(...)
        21.                           │   └─coro (local) user(...)
        22.                           │     ├─.last_value <<- eval_bare(substitute(expr), user_env)
        23.                           │     │ └─rlang::env_poke(env, lhs, value, inherit = TRUE, create = FALSE)
        24.                           │     └─rlang::eval_bare(substitute(expr), user_env)
        25.                           └─(utils::getFromNamespace("record_chat_otel_span_status", "ellmer"))(...)
        26.                             └─ellmer:::otel_metric_attributes(provider, model, result)
       
       [ FAIL 46 | WARN 0 | SKIP 2 | PASS 394 ]
       Error:
       ! Test failures.
       Warning message:
       In for (i in seq_along(cenv$extra)) { :
         closing unused connection 4 (charToRaw(paste0(fixture_text(name), "\n\n")))
       Execution halted
     ```

# vitals (0.4.0)

* GitHub: <https://github.com/tidyverse/vitals>
* Email: <mailto:simon.couch@posit.co>
* GitHub mirror: <https://github.com/cran/vitals>

Run `revdepcheck::cloud_details(, "vitals")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
            ▆
         1. └─tsk$solve() at test-task.R:1135:3
         2.   └─private$solver(self$get_samples()$input, ...)
         3.     └─vitals (local) fn(...)
         4.       └─ellmer::parallel_chat(ch, as.list(inputs), ...)
         5.         └─ellmer (local) my_parallel_turns(conversations)
         6.           └─ellmer:::parallel_turns(...)
         7.             └─httr2::req_perform_parallel(reqs, max_active = max_active, on_error = on_error)
         8.               ├─base::tryCatch(...)
         9.               │ └─base (local) tryCatchList(expr, classes, parentenv, handlers)
        10.               │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
        11.               │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
        12.               └─queue$process()
        13.                 └─self$process1(deadline)
        14.                   └─self$submit_next(deadline)
        15.                     └─self$pooled_reqs[[i]]$submit(self$pool)
        16.                       └─private$mock(self$req)
        17.                         └─RequestHandlerHttr2$new(req)$handle()
        18.                           └─cli::cli_abort(...)
        19.                             └─rlang::abort(...)
       
       [ FAIL 26 | WARN 0 | SKIP 61 | PASS 152 ]
       Error:
       ! Test failures.
       Execution halted
     ```

