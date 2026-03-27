# Generate presentation images from a YAML configuration

Generate presentation images from a YAML configuration

## Usage

``` r
bananarama(path = "bananarama.yaml", output_dir = NULL, force = FALSE)
```

## Arguments

- path:

  Path to a YAML configuration file or a directory containing
  `bananarama.yaml`. Defaults to `"bananarama.yaml"` in the current
  directory.

- output_dir:

  Directory to save generated images, relative to the YAML configuration
  file (or an absolute path). Defaults to the `output-dir` field in the
  YAML file, or a directory with the same name as the YAML file (e.g.
  `bananarama.yaml` outputs to `bananarama/`).

- force:

  If `TRUE`, regenerate all images even if they already exist.

## Value

Invisibly returns a character vector of output file paths.

## Examples

``` r
if (FALSE) { # \dontrun{
bananarama("demo/")
bananarama("demo/bananarama.yaml")
} # }
```
