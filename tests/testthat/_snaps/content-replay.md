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
      ! Cannot record or replay a <foo::LocalClass> object.
      i Only `ellmer::Content` or `ellmer::Turn` classes or subclasses are currently supported.
    Code
      contents_replay(recorded)
    Condition
      Error in `loadNamespace()`:
      ! there is no package called 'foo'

# local classes that extend ellmer classes can be replayed

    Code
      test_record_replay(test_content("hello world"))
    Condition
      Error in `contents_replay()`:
      ! Expected the object named `LocalContentText` to be an S7 class, not a function.

# replayed objects must be existing S7 classes

    Code
      contents_replay(doesnt_exist)
    Condition
      Error in `contents_replay()`:
      ! Unable to find the S7 class: `ellmer::Turn2`.
    Code
      contents_replay(not_s7)
    Condition
      Error in `contents_replay()`:
      ! Expected the object named `ellmer::chat_openai` to be an S7 class, not a function.

