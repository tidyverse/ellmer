# df_schema aggregates column descriptions

    Code
      df_schema(df)
    Output
      [1] | A data frame with 3 rows and 2 columns:
          | * x: integer with range [1, 3], and 0 NAs
          | * y: character with 0 NAs, and 3 unique values ("a", "b", "c")

# df_schema works on edge case data frames

    Code
      df_schema(data.frame())
    Output
      [1] | A data frame with 0 rows and 0 columns:
    Code
      df_schema(data.frame(x = 1, y = "a"))
    Output
      [1] | A data frame with 1 rows and 2 columns:
          | * x: numeric with range [1, 1], and 0 NAs
          | * y: character with 0 NAs, and 1 unique values ("a")
    Code
      df_schema(data.frame(x = 1:5))
    Output
      [1] | A data frame with 5 rows and 1 columns:
          | * x: integer with range [1, 5], and 0 NAs

# df_schema checks its inputs

    Code
      df_schema(1)
    Condition
      Error in `df_schema()`:
      ! `df` must be a data frame, not the number 1.

# warns for wide data frames

    Code
      df_schema(df, max_cols = 5)
    Condition
      Warning:
      Truncating to 5 columns.
    Output
      [1] | A data frame with 1 rows and 26 columns:
          | * a: numeric with range [1, 1], and 0 NAs
          | * b: numeric with range [1, 1], and 0 NAs
          | * c: numeric with range [1, 1], and 0 NAs
          | * d: numeric with range [1, 1], and 0 NAs
          | * e: numeric with range [1, 1], and 0 NAs
          | and 0 more columns

# col_schema handles logical vectors

    Code
      col_schema(c(TRUE, FALSE, TRUE, NA))
    Output
      [1] | logical with 2 TRUEs, 1 FALSEs, and 1 NAs

# col_schema handles numeric vectors

    Code
      col_schema(c(1L:5L, NA))
    Output
      [1] | integer with range [1, 5], and 1 NAs
    Code
      col_schema(c(1.5, 2.5, 3.5))
    Output
      [1] | numeric with range [1.5, 3.5], and 0 NAs
    Code
      col_schema(c(1.5, NA, NaN, Inf, -Inf, 3.5))
    Output
      [1] | numeric with range [-Inf, Inf], and 2 NAs

# col_schema handles characters and factors

    Code
      col_schema(c("a", "b", NA))
    Output
      [1] | character with 1 NAs, and 2 unique values ("a", "b")
    Code
      col_schema(letters)
    Output
      [1] | character with 0 NAs, and 26 unique values
    Code
      col_schema(factor(c("a", "b", NA)))
    Output
      [1] | nominal with 1 NAs, and 2 permitted values ("a", "b")
    Code
      col_schema(factor(c("a", "b", NA), exclude = NULL))
    Output
      [1] | nominal with 0 NAs, and 2 permitted values ("a", "b")
    Code
      col_schema(ordered(c("low", "med", "high")))
    Output
      [1] | ordinal with 0 NAs, and 3 permitted values ("high", "low", "med")

# col_schema handles date/times

    Code
      col_schema(as.Date(c("2024-01-01", "2024-12-31", NA)))
    Output
      [1] | date with range [2024-01-01, 2024-12-31], and 1 NAs
    Code
      col_schema(as.POSIXct(c("2024-01-01 10:30:00", "2024-12-31 23:59:59")))
    Output
      [1] | date-time with range [2024-01-01 10:30:00, 2024-12-31 23:59:59], and 0 NAs
    Code
      col_schema(as.POSIXct("2024-01-01 10:30:00", tz = "America/New_York"))
    Output
      [1] | date-time with timezone America/New_York, range [2024-01-01 10:30:00, 2024-01-01 10:30:00], and 0 NAs

# col_schema handles empty vectors

    Code
      col_schema(character(0))
    Output
      [1] | character with 0 NAs, and 0 unique values
    Code
      col_schema(numeric(0))
    Output
      [1] | numeric with 0 NAs
    Code
      col_schema(logical(0))
    Output
      [1] | logical with 0 TRUEs, 0 FALSEs, and 0 NAs
    Code
      col_schema(factor(character(0)))
    Output
      [1] | nominal with 0 NAs, and 0 permitted values
    Code
      col_schema(as.Date(character(0)))
    Output
      [1] | date with 0 NAs
    Code
      col_schema(as.POSIXct(character(0)))
    Output
      [1] | date-time with 0 NAs

# col_schema handles data frame columns

    Code
      col_schema(df)
    Output
      [1] | data frame with a, b, and 0 NAs

# col_schema handles list columns

    Code
      col_schema(lst)
    Output
      [1] | list column

# col_schema handles unknown classes

    Code
      col_schema(x)
    Output
      [1] | class1/class2/class3

# col_schema handles labeled columns

    Code
      col_schema(x)
    Output
      [1] | integer with label (My labeled variable), range [1, 5], and 0 NAs

# desc_range handles various numeric inputs

    Code
      desc_range(numeric())
    Output
      NULL
    Code
      desc_range(c(-Inf, 1, 2, Inf))
    Output
      [1] "range [-Inf, Inf]"
    Code
      desc_range(c(NA, 1, 2, NA))
    Output
      [1] "range [1, 2]"

# desc_unique handles character vectors

    Code
      desc_unique(letters)
    Output
      [1] "26 unique values"
    Code
      desc_unique(strrep("x", 10000))
    Output
      [1] "1 unique values"
    Code
      desc_unique(character())
    Output
      [1] "0 unique values"

