# Authenticate with the Vantor / Maxar API

Sets or retrieves authentication credentials for the current R session.
Supports both long-lived API keys (`maxar-api-key`) and OAuth2 Bearer
tokens (`Authorization: Bearer <token>`) with automatic refresh when
expired.

## Usage

``` r
vantor_auth(
  api_key = NULL,
  username = NULL,
  password = NULL,
  token = NULL,
  refresh_token = NULL,
  verbose = getOption("vantorr.verbose", TRUE)
)
```

## Arguments

- api_key:

  Character string. A Vantor/Maxar API key.

- username:

  Character string. Email address for OAuth2 authentication.

- password:

  Character string. Password for OAuth2 authentication.

- token:

  Character string. An existing OAuth2 access token.

- refresh_token:

  Character string. An existing OAuth2 refresh token.

- verbose:

  Logical. Print status messages. Defaults to
  `getOption("vantorr.verbose", TRUE)`.

## Value

Invisibly returns a list representing the active authentication state,
or `NULL` if no credentials could be resolved.

## Details

By default, `vantor_auth()` checks the following environment variables
if arguments are not explicitly supplied:

- `VANTOR_API_KEY` or `MAXAR_API_KEY`

- `VANTOR_USERNAME` or `MAXAR_USERNAME`

- `VANTOR_PASSWORD` or `MAXAR_PASSWORD`

## Note

**Authentication Scope Across Vantor Endpoints:**

- **OGC & Non-Administrative APIs**
  ([`get_maxar_wms_basemap()`](https://michael-cw.github.io/vantorr/reference/get_maxar_wms_basemap.md),
  [`get_maxar_wms_tiles()`](https://michael-cw.github.io/vantorr/reference/get_maxar_wms_tiles.md),
  [`vantor_get_seamlines()`](https://michael-cw.github.io/vantorr/reference/vantor_get_seamlines.md)):
  Accept **either** an API key (`maxar-api-key`) or an OAuth2 Bearer
  token.

- **Browser-Based `leaflet` Tile Layers**
  ([`add_vantor_basemap()`](https://michael-cw.github.io/vantorr/reference/add_vantor_basemap.md),
  [`add_vantor_seamlines()`](https://michael-cw.github.io/vantorr/reference/add_vantor_seamlines.md),
  [`add_vantor_imagery()`](https://michael-cw.github.io/vantorr/reference/add_vantor_imagery.md),
  [`vantor_wmts_url()`](https://michael-cw.github.io/vantorr/reference/vantor_wmts_url.md)):
  Require an **API key** (`maxar_api_key` URL query parameter).

- **Account Services / Admin APIs**
  ([`vantor_get_usage()`](https://michael-cw.github.io/vantorr/reference/vantor_get_usage.md)):
  Require an **OAuth2 Bearer token** (`username` and `password`, or
  `token`). Per the Vantor API specification, Admin APIs do not accept
  API keys.

## Examples

``` r
# Check if credentials are configured in the environment
vantor_has_auth(verbose = FALSE)
#> [1] FALSE

# Set an API key for the current session
vantor_auth(api_key = "your_api_key_here", verbose = FALSE)

# Clear session credentials
vantor_auth_clear(verbose = FALSE)
```
