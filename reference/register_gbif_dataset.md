# Register a hosted archive on GBIF

Create a dataset entry on GBIF and point it to your public DwC-A zip.
Credentials and keys are read from `inst/config.yml`
(`gbif$production`). Reads `GBIF_ENDPOINT_URL`, `GBIF_USERNAME`,
`GBIF_PASSWORD`, `GBIF_ORG_KEY`, and `GBIF_INSTALL_KEY` from the
environment.

## Usage

``` r
register_gbif_dataset(
  title,
  description,
  license = "CC_BY_NC_4_0",
  language = "eng",
  type = "OCCURRENCE"
)
```

## Arguments

- title:

  Dataset title.

- description:

  Brief dataset description.

- license:

  GBIF license code. Default: "CC_BY_NC_4_0".

- language:

  ISO language code. Default: "eng".

- type:

  Dataset type. Default: "OCCURRENCE".

## Value

List with `dataset_key`, `registration`, and `endpoint`.
