# GBIF-Test demo flow (fixed keys and credentials)

Runs the GBIF-Test demo recipe: creates a dataset with the documented
demo org/install keys and adds your DwC-A URL as the endpoint. No
lookups needed. Use only on GBIF-Test with the demo credentials.

## Usage

``` r
register_gbif_dataset_test(
  endpoint_url,
  title = "Example dataset registration",
  description = "Minimal metadata; overwritten after GBIF fetches the archive.",
  type = "OCCURRENCE",
  license = "http://creativecommons.org/publicdomain/zero/1.0/legalcode",
  language = "eng"
)
```

## Arguments

- endpoint_url:

  Public URL to the DwC-A zip file.

- title:

  Dataset title.

- description:

  Brief dataset description.

- type:

  Dataset type. Default: "OCCURRENCE".

- license:

  License URL. Default:
  "http://creativecommons.org/publicdomain/zero/1.0/legalcode".

- language:

  ISO language code. Default: "eng".

## Value

List with `dataset_key`, `registration`, and `endpoint`.
