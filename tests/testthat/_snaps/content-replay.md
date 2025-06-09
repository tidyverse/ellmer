# unknown classes cause errors

    Code
      contents_replay(recorded, chat = chat)
    Condition
      Error in `get_cls_constructor()`:
      ! Unable to find the S7 class: "ellmer::Turn2".

# replay classes are S7 classes

    Code
      contents_replay(recorded, chat = chat)
    Condition
      Error in `get_cls_constructor()`:
      ! The object returned for "LocalClass" is not an S7 class.

