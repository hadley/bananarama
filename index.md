# bananarama

bananarama generates slide backgrounds with Google Gemini. Define your
images in a YAML configuration file with support for reference images
and style defaults, then generate images in parallel, making it fast to
generate full deck of images.

bananarama avoids the usual tedious loop of copying prompts into a web
UI, downloading images, tweaking, and repeating. Now your process is
(mostly) reproducible: your prompts live in version-controlled YAML, and
regenerating every image is a single function call. No more copy-paste
and no more losing track of which prompt produced which image.

## Installation

You can install the development version of bananarama from GitHub:

``` r
# install.packages("pak")
pak::pak("hadley/bananarama")
```

## Usage

Create a `bananarama.yaml` file that describes the images you want to
generate:

``` yaml
defaults:
  style: >
    Flat vector editorial illustration with a muted, desaturated
    color palette. Mid-century modern aesthetic. Calm and approachable.

images:
  - name: robot-factory
    description: >
      Draw a picture of [hadley] overseeing a factory full of robots.
      The robots should be typing at computers.
```

Reference images like `[hadley]` are matched to image files
(e.g. `hadley.png`) in the same directory as the YAML file.

Then generate the images with:

``` r
bananarama::bananarama("path/to/bananarama.yaml")
```

Images that already exist are skipped unless you pass `force = TRUE`.

Bananrama currently uses nano banana2 (akaa
`gemini-3.1-flash-image-preview`), which costs around \$0.07 per image.

## YAML configuration

### `defaults`

- **`style`**: Style prompt appended to every image description. Use
  this to ensure a consistent style across all slides.
- **`description`**: Default description; not usually needed unless you
  want to experiment with styles.
- **`aspect-ratio`**: One of `"1:1"`, `"3:2"`, `"16:9"`, etc. Default:
  `"16:9"`.
- **`resolution`**: One of `"1K"`, `"2K"`, `"4K"`. Default: `"1K"`.
- **`n`**: Number of variants to generate per image. Default: `1`.
- **`model`**: Gemini model to use. Default:
  `"gemini-3.1-flash-image-preview"` (aka nano banana2).
- **`force`**: If `true`, regenerate images even if they already exist.
  Default: `false`.
- **`seed`**: Integer seed for the random number generator. Makes output
  more (but not perfectly) deterministic.

### `output-dir`

Output directory for generated images, relative to the YAML file.
Defaults to a directory with the same name as the YAML file
(e.g. `bananarama.yaml` outputs to `bananarama/`).

### `images`

Each image has:

- **`name`** (required): Used as the output filename (`{name}.png`).
- **`description`** (required, unless a default is set): Prompt for
  image generation. Use `[name]` to reference images in the same
  directory.
- **`n`**: Number of variants to generate. Output files are named
  `{name}-1.png`, `{name}-2.png`, etc. Default: `1`.
- **`style`**, **`aspect-ratio`**, **`resolution`**, **`model`**,
  **`force`**, **`seed`**: Per-image overrides of the defaults.
