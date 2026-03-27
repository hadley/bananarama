resolve_config_path <- function(path) {
  path <- path.expand(path)
  if (dir.exists(path)) {
    path <- file.path(path, "bananarama.yaml")
  }

  if (!file.exists(path)) {
    cli::cli_abort("Cannot find config file {.path {path}}.")
  }

  path
}

parse_image_config <- function(config_path) {
  config <- yaml12::read_yaml(config_path)

  defaults <- parse_defaults(config$defaults)
  images <- parse_images(config$images, defaults)

  list(
    images = images,
    base_dir = dirname(config_path),
    output_dir = config$`output-dir`
  )
}

parse_defaults <- function(values) {
  defaults <- list(
    model = "gemini-3.1-flash-image-preview",
    description = NULL,
    style = NULL,
    `aspect-ratio` = "16:9",
    resolution = "1K",
    n = 1L
  )

  utils::modifyList(defaults, values %||% list())
}

parse_images <- function(images, defaults) {
  if (is.null(images)) {
    cli::cli_abort("Configuration must contain an {.field images} field.")
  }

  lapply(images, parse_image, defaults = defaults)
}

parse_image <- function(img, defaults) {
  if (is.null(img$name)) {
    cli::cli_abort("Each image must have a {.field name} field.")
  }
  description <- img$description %||% defaults$description
  if (is.null(description)) {
    cli::cli_abort(
      "Image {.val {img$name}} must have a {.field description} field."
    )
  }

  aspect_ratio <- img$`aspect-ratio` %||% defaults$`aspect-ratio`
  check_aspect_ratio(aspect_ratio, img$name)

  resolution <- img$resolution %||% defaults$resolution
  check_resolution(resolution, img$name)

  n <- as.integer(img[["n"]] %||% defaults$n %||% 1L)
  if (n < 1L) {
    cli::cli_abort("Image {.val {img$name}} must have {.field n} >= 1.")
  }

  list(
    name = img$name,
    description = description,
    model = img$model %||% defaults$model,
    style = img$style %||% defaults$style,
    `aspect-ratio` = aspect_ratio,
    resolution = resolution,
    n = n
  )
}

check_aspect_ratio <- function(value, name) {
  valid <- c(
    "1:1",
    "2:3",
    "3:2",
    "3:4",
    "4:3",
    "4:5",
    "5:4",
    "9:16",
    "16:9",
    "21:9"
  )
  if (!value %in% valid) {
    cli::cli_abort(c(
      "Image {.val {name}} has invalid {.field aspect-ratio} {.val {value}}.",
      i = "Must be one of {.or {.val {valid}}}."
    ))
  }
}

check_resolution <- function(value, name) {
  valid <- c("1K", "2K", "4K")
  if (!value %in% valid) {
    cli::cli_abort(c(
      "Image {.val {name}} has invalid {.field resolution} {.val {value}}.",
      i = "Must be one of {.or {.val {valid}}}."
    ))
  }
}

resolve_placeholders <- function(description, base_dir, start_index = 0) {
  if (is.null(description)) {
    return(list(text = NULL, images = character()))
  }

  # Find all [name] patterns

  pattern <- "\\[([^\\]]+)\\]"
  matches <- gregexpr(pattern, description, perl = TRUE)
  match_data <- regmatches(description, matches)[[1]]

  if (length(match_data) == 0) {
    return(list(text = description, images = character()))
  }

  # Extract names from brackets
  names <- gsub("^\\[|\\]$", "", match_data)

  # Find image files and build replacements
  images <- character()
  text <- description

  for (i in seq_along(names)) {
    name <- names[[i]]
    image_path <- find_image_file(name, base_dir)
    images <- c(images, image_path)

    # Replace [name] with "name (shown in image N)"
    ordinal <- start_index + i
    replacement <- paste0(name, " (shown in image ", ordinal, ")")
    text <- sub(paste0("\\[", name, "\\]"), replacement, text, fixed = FALSE)
  }

  list(text = text, images = images)
}

find_image_file <- function(name, base_dir) {
  extensions <- c(".png", ".jpg", ".jpeg", ".webp", ".gif")

  for (ext in extensions) {
    path <- file.path(base_dir, paste0(name, ext))
    if (file.exists(path)) {
      return(path)
    }
  }

  cli::cli_abort(c(
    "Cannot find reference image for {.val {name}}.",
    i = "Looked for {.file {name}.png}, {.file {name}.jpg}, etc. in {.path {base_dir}}."
  ))
}
