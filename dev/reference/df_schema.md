# Describe the schema of a data frame, suitable for sending to an LLM

`df_schema()` gives a column-by-column description of a data frame. For
each column, it gives the name, type, label (if present), and number of
missing values. For numeric and date/time columns, it also gives the
range. For character and factor columns, it also gives the number of
unique values, and if there's only a few (\<= 10), their values.

The goal is to give the LLM a sense of the structure of the data, so
that it can generate useful code, and the output attempts to balance
between conciseness and accuracy.

## Usage

``` r
df_schema(df, max_cols = 50)
```

## Arguments

- df:

  A data frame to describe.

- max_cols:

  Maximum number of columns to includes. Defaults to 50 to avoid
  accidentally generating very large prompts.

## Examples

``` r
df_schema(mtcars)
#> [1] │ A data frame with 32 rows and 11 columns:
#>     │ * mpg: numeric with range [10.4, 33.9], and 0 NAs
#>     │ * cyl: numeric with range [4, 8], and 0 NAs
#>     │ * disp: numeric with range [71.1, 472], and 0 NAs
#>     │ * hp: numeric with range [52, 335], and 0 NAs
#>     │ * drat: numeric with range [2.76, 4.93], and 0 NAs
#>     │ * wt: numeric with range [1.513, 5.424], and 0 NAs
#>     │ * qsec: numeric with range [14.5, 22.9], and 0 NAs
#>     │ * vs: numeric with range [0, 1], and 0 NAs
#>     │ * am: numeric with range [0, 1], and 0 NAs
#>     │ * gear: numeric with range [3, 5], and 0 NAs
#>     │ * carb: numeric with range [1, 8], and 0 NAs
df_schema(iris)
#> [1] │ A data frame with 150 rows and 5 columns:
#>     │ * Sepal.Length: numeric with range [4.3, 7.9], and 0 NAs
#>     │ * Sepal.Width: numeric with range [2, 4.4], and 0 NAs
#>     │ * Petal.Length: numeric with range [1, 6.9], and 0 NAs
#>     │ * Petal.Width: numeric with range [0.1, 2.5], and 0 NAs
#>     │ * Species: nominal with 0 NAs, and 3 permitted values ("setosa", "versicolor", "virginica")
```
