# Prepare repeat answers from Kobo survey forms

Takes the repeating sample answers in MC survey exports (as used in
[`preprocess_surveys()`](https://ioledc.github.io/ZOOGoN-40Y/reference/preprocess_surveys.md))
and lays them out so each repeat sits on its own line, making the survey
easier to read and join with other info.

## Usage

``` r
reshape_kobo_repeat(data, group_name, id_col = "submission_id")
```

## Arguments

- data:

  Downloaded Kobo survey data frame.

- group_name:

  Prefix of the repeat group to unfold (e.g. `"group_taxa"`).

- id_col:

  Column used to order results (default: `submission_id`).

## Value

Data frame with one row per submission and repeat entry.

## Details

Technical: pivots columns starting with the repeat group prefix, splits
out the repeat number and field name, spreads them back to columns, and
orders the rows by the ID column and repeat number.
