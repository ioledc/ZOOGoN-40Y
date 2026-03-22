# Render ZooGoN MC Survey Report

This function renders the Quarto report using the preprocessed survey
data. It downloads the latest preprocessed data from SharePoint and
renders the report to HTML format, saving it in the working directory.

## Usage

``` r
render_report(output_dir = "/home")
```

## Arguments

- output_dir:

  Directory where the rendered report will be saved. Defaults to the
  current working directory.

## Value

Invisible NULL. Renders the report to the output directory.

## Details

The function performs the following steps:

1.  Reads configuration settings from config.yml

2.  Locates Report.qmd inside the installed package (inst/report/)

3.  Renders the Quarto report to HTML in the specified output directory

## Examples

``` r
if (FALSE) { # \dontrun{
render_report()
} # }
```
