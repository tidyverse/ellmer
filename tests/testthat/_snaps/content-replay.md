# non-ellmer classes are recorded/replayed by default

    Code
      contents_record(LocalClass("testname"), chat = chat)
    Condition
      Error in `contents_record()`:
      ! Only S7 classes from the `ellmer` package are currently supported. Received: "LocalClass".

---

    Code
      contents_replay(list(version = 1, class = "testpkg::LocalClass", props = list(
        name = "testname")), chat = chat)
    Condition
      Error in `contents_replay()`:
      ! Only S7 classes from the `ellmer` package are currently supported.

# unknown classes cause errors

    Code
      contents_replay(recorded, chat = chat)
    Condition
      Error in `contents_replay()`:
      ! Unable to find the S7 class: "ellmer::Turn2".

