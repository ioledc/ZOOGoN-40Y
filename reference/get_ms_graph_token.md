# Get Microsoft Graph API access token

Authenticates with Microsoft using client credentials (app registration)
and returns an access token for API calls.

## Usage

``` r
get_ms_graph_token(sp_conf)
```

## Arguments

- sp_conf:

  SharePoint configuration list with tenant_id, client_id, and
  client_secret

## Value

Access token string
