# Register a hosted archive on GBIF

Create a dataset entry in GBIF and point it to your public DwC-A zip.
You must supply valid GBIF organization and installation UUIDs and a URL
that anyone can download.

## Usage

``` r
register_gbif_dataset(
  endpoint_url,
  organization_key,
  installation_key,
  title,
  description,
  username,
  password,
  license = "CC_BY_NC_4_0",
  language = "eng",
  type = "OCCURRENCE"
)
```

## Arguments

- endpoint_url:

  Public URL to the DwC-A zip file.

- organization_key:

  GBIF publishing organization UUID.

- installation_key:

  GBIF installation UUID (e.g., from IPT).

- title:

  Dataset title.

- description:

  Brief dataset description.

- username:

  GBIF account username.

- password:

  GBIF account password.

- license:

  GBIF license code. Default: "CC_BY_NC_4_0".

- language:

  ISO language code. Default: "eng".

- type:

  Dataset type. Default: "OCCURRENCE".

## Value

Parsed JSON response from GBIF.
