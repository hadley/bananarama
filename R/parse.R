# Supported models and their providers. The provider is looked up from the
# model name, so the YAML config never needs a separate provider field.
model_registry <- list(
  "gemini-3.1-flash-image-preview" = list(provider = "gemini"),
  "gemini-3.1-flash-lite-image" = list(provider = "gemini"),
  "gemini-3-pro-image" = list(provider = "gemini"),
  "gpt-image-2.5-flare" = list(provider = "openai"),
  "gpt-image-2.5-flare-2026-09-08" = list(provider = "openai"),
  "gpt-image-2.5-sunburst" = list(provider = "openai"),
  "gpt-image-2.5-sunburst-2026-09-08" = list(provider = "openai")
)

check_model <- function(value, name) {
  entry <- model_registry[[value]]
  if (is.null(entry)) {
    cli::cli_abort(c(
      "Image {.val {name}} has unsupported {.field model} {.val {value}}.",
      i = "Must be one of {.or {.val {names(model_registry)}}}."
    ))
  }
  entry$provider
}

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
    n = 1L,
    force = FALSE,
    seed = NULL
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

  n <- as.integer(img[["n"]] %||% defaults$n %||% 1L)
  if (n < 1L) {
    cli::cli_abort("Image {.val {img$name}} must have {.field n} >= 1.")
  }

  force <- img$force %||% defaults$force %||% FALSE
  spec <- resolve_spec(img, defaults, img$name)

  if (!is.null(img$sequence)) {
    steps <- parse_steps(img$sequence, spec, path = img$name)
    return(list(
      name = img$name,
      sequence = steps,
      resolution = spec$resolution,
      n = n,
      force = force
    ))
  }

  description <- img$description %||% defaults$description
  if (is.null(description)) {
    cli::cli_abort(
      "Image {.val {img$name}} must have a {.field description} field."
    )
  }

  c(
    list(name = img$name, description = description),
    spec,
    list(
      n = n,
      force = force
    )
  )
}

# Resolve the overridable fields (model, style, aspect-ratio, resolution,
# seed) for an image or sequence step against its inherited values.
resolve_spec <- function(values, defaults, name) {
  aspect_ratio <- values$`aspect-ratio` %||% defaults$`aspect-ratio`
  check_aspect_ratio(aspect_ratio, name)

  resolution <- values$resolution %||% defaults$resolution
  check_resolution(resolution, name)

  model <- values$model %||% defaults$model
  provider <- check_model(model, name)

  seed <- values$seed %||% defaults$seed
  if (!is.null(seed) && provider == "openai") {
    cli::cli_warn(c(
      "Image {.val {name}} sets {.field seed}, which {.val {model}} does not support.",
      i = "The seed will be ignored."
    ))
    seed <- NULL
  }

  list(
    model = model,
    provider = provider,
    style = values$style %||% defaults$style,
    `aspect-ratio` = aspect_ratio,
    resolution = resolution,
    seed = seed
  )
}

parse_steps <- function(steps, inherited, path) {
  nms <- vapply(
    steps,
    function(step) step$name %||% NA_character_,
    character(1)
  )
  if (anyNA(nms)) {
    cli::cli_abort(
      "Each step in sequence {.val {path}} must have a {.field name} field."
    )
  }
  if (anyDuplicated(nms)) {
    dup <- unique(nms[duplicated(nms)])
    cli::cli_abort(
      "Sequence {.val {path}} has duplicate step name{?s} {.val {dup}}."
    )
  }

  lapply(steps, parse_step, inherited = inherited, path = path)
}

parse_step <- function(step, inherited, path) {
  full_name <- paste(path, step$name, sep = "-")

  if (is.null(step$description) && is.null(step$sequence)) {
    cli::cli_abort(
      "Step {.val {full_name}} must have a {.field description} and/or a {.field sequence}."
    )
  }
  if (!is.null(step$resolution)) {
    cli::cli_abort(c(
      "Step {.val {full_name}} sets {.field resolution}.",
      i = "{.field resolution} can only be set at the top level of an image."
    ))
  }
  if (!is.null(step[["n"]])) {
    cli::cli_abort(c(
      "Step {.val {full_name}} sets {.field n}.",
      i = "{.field n} can only be set at the top level of an image."
    ))
  }

  spec <- resolve_spec(step, inherited, full_name)
  children <- if (!is.null(step$sequence)) {
    parse_steps(step$sequence, spec, path = full_name)
  }

  c(
    list(
      name = step$name,
      full_name = full_name,
      description = step$description
    ),
    spec,
    list(sequence = children)
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
