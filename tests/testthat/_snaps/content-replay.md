# checks recorded value types

    Code
      contents_replay(bad_names)
    Condition
      Error in `contents_replay()`:
      ! Expected the recorded object to be a list with at least names 'version', 'class', and 'props'.
    Code
      contents_replay(bad_version)
    Condition
      Error in `check_recorded()`:
      ! Unsupported version 2.
    Code
      contents_replay(bad_class)
    Condition
      Error in `contents_replay()`:
      ! Expected the recorded object to have a single $class name, containing `::` if the class is from a package.

# non-ellmer classes are not recorded/replayed by default

    Code
      contents_record(LocalClass())
    Condition
      Error in `contents_record()`:
      ! Only S7 classes from the `ellmer` package are currently supported. Received: "foo::LocalClass".
    Code
      contents_replay(recorded)
    Condition
      Error in `contents_replay()`:
      ! Only S7 classes from the `ellmer` package are currently supported. Received: "foo::LocalClass".

# replayed objects must be existing S7 classes

    Code
      contents_replay(doesnt_exist)
    Condition
      Error in `contents_replay()`:
      ! Unable to find the S7 class: "ellmer::Turn2".
    Code
      contents_replay(not_s7)
    Condition
      Error in `contents_replay()`:
      ! The object returned for "ellmer::chat_openai" is not an S7 class.

