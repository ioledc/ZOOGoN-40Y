# Run the ZooGoN survey-to-report pipeline

Runs the survey ingestion and report rendering steps in sequence:
ingestion -\> preprocessing -\> report rendering. This covers the
reporting branch of the pipeline. To also build the Darwin Core Archive,
call
[`format_to_tidy()`](https://ioledc.github.io/ZOOGoN-40Y/reference/format_to_tidy.md)
followed by
[`format_to_DC_archive()`](https://ioledc.github.io/ZOOGoN-40Y/reference/format_to_DC_archive.md).

## Usage

``` r
run_pipeline()
```

## Value

Invisible NULL.

## Examples

``` r
if (FALSE) { # \dontrun{
run_pipeline()
} # }
```
