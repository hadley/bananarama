# bananarama

<!-- badges: start -->
<!-- badges: end -->

bananarama generates presentation images using Google Gemini's image generation capabilities. Define your images in a YAML configuration file with support for reference images and style defaults.

## Installation

You can install the development version of bananarama from [GitHub](https://github.com/hadley/bananarama):

``` r
# install.packages("pak")
pak::pak("hadley/bananarama")
```

## Usage

Create a `bananarama.yaml` file that describes the images you want to generate:

``` yaml
defaults:
  style: >
    Flat vector editorial illustration with a muted, desaturated
    color palette. Mid-century modern aesthetic. Calm and approachable.
  aspect-ratio: 16:9

images:
  - name: robot-factory
    description: >
      Draw a picture of [hadley] overseeing a factory full of robots.
      The robots should be typing at computers.
```

Reference images like `[hadley]` are matched to image files (e.g. `hadley.png`) in the same directory as the YAML file.

Then generate the images with:

``` r
bananarama::bananarama("path/to/bananarama.yaml")
```

Images that already exist are skipped unless you pass `force = TRUE`.

## YAML configuration

### `defaults`

- **`style`**: Style prompt appended to every image description.
- **`description`**: Default description (useful if you just want to experiment with styles).
- **`aspect-ratio`**: One of `"1:1"`, `"3:2"`, `"16:9"`, etc. Default: `"16:9"`.
- **`resolution`**: One of `"1K"`, `"2K"`, `"4K"`. Default: `"1K"`.
- **`n`**: Number of variants to generate per image. Default: `1`.
- **`model`**: Gemini model to use. Default: `"gemini-3.1-flash-image-preview"`.

### `output-dir`

Output directory for generated images, relative to the YAML file. Defaults to a directory with the same name as the YAML file (e.g. `bananarama.yaml` outputs to `bananarama/`).

### `images`

Each image has:

- **`name`** (required): Used as the output filename (`{name}.png`).
- **`description`** (required, unless a default is set): Prompt for image generation. Use `[name]` to reference images in the same directory.
- **`n`**: Number of variants to generate. Output files are named `{name}-1.png`, `{name}-2.png`, etc. Default: `1`.
- **`style`**, **`aspect-ratio`**, **`resolution`**, **`model`**: Per-image overrides of the defaults.
