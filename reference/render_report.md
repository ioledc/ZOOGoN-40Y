# Render ZooGoN MC Survey Report

This function renders the Quarto monitoring report from the installed
package, saves the HTML output with a versioned filename, and uploads it
to the SharePoint reports bucket. The report reads the merged tidy
dataset directly from SharePoint via
[`read_config()`](https://ioledc.github.io/ZOOGoN-40Y/reference/read_config.md).

## Usage

``` r
render_report(output_dir = "/home")
```

## Arguments

- output_dir:

  Local directory where the rendered HTML is saved before upload.
  Defaults to `"/home"`.

## Value

Invisible NULL.

## Details

The function performs the following steps:

1.  Reads configuration from `inst/config.yml` via
    [`read_config()`](https://ioledc.github.io/ZOOGoN-40Y/reference/read_config.md).

2.  Locates `REPORT_interact.qmd` inside the installed package.

3.  Renders the Quarto report to HTML.

4.  Copies the output to `output_dir` with a versioned filename produced
    by
    [`add_version()`](https://ioledc.github.io/ZOOGoN-40Y/reference/add_version.md)
    (timestamp + git SHA).

5.  Uploads the HTML to the SharePoint `reports` bucket.

## Examples

``` r
if (FALSE) { # \dontrun{
render_report()
} # }
```
