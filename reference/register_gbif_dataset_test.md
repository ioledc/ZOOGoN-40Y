# GBIF-Test registration flow

Registers a dataset on the GBIF-Test sandbox using credentials and keys
from `inst/config.yml` (`gbif$test`). Reads `GBIF_TEST_ENDPOINT_URL`,
`GBIF_TEST_USER`, `GBIF_TEST_PASSWORD`, `GBIF_TEST_ORG_KEY`, and
`GBIF_TEST_INSTALL_KEY` from the environment.

## Usage

``` r
register_gbif_dataset_test(
  title = "Example dataset registration",
  description = "Minimal metadata; overwritten after GBIF fetches the archive.",
  type = "OCCURRENCE",
  license = "http://creativecommons.org/publicdomain/zero/1.0/legalcode",
  language = "eng"
)
```

## Arguments

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
