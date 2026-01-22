#' Generate presentation images from a YAML configuration
#'
#' @param path Path to a YAML configuration file or a directory containing
#'   `bananarama.yaml`. Defaults to `"bananarama.yaml"` in the current directory.
#' @param output_dir Directory to save generated images. Defaults to an
#'   `output/` subdirectory next to the configuration file.
#' @param force If `TRUE`, regenerate all images even if they already exist.
#' @return Invisibly returns a character vector of output file paths.
#' @export
#' @examples
#' \dontrun{
#' bananarama("demo/")
#' bananarama("demo/bananarama.yaml")
#' }
bananarama <- function(
  path = "bananarama.yaml",
  output_dir = NULL,
  force = FALSE
) {
  config_path <- resolve_config_path(path)
  config <- parse_image_config(config_path)

  if (is.null(output_dir)) {
    output_dir <- file.path(config$base_dir, "output")
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Sort images by dependencies and preprocess

  images <- sort_by_dependencies(config$images)
  images <- preprocess_images(images, config$base_dir, output_dir)

  output_paths <- character()
  for (image in images) {
    output_paths <- c(output_paths, image$output_path)

    if (!force && file.exists(image$output_path)) {
      cli::cli_alert_info("Skipping {.val {image$name}} (already exists)")
      next
    }

    cli::cli_alert("Generating {.val {image$name}}...")
    generate_single_image(image, images)
    cli::cli_alert_success("Generated {.val {image$name}}")
  }

  invisible(output_paths)
}

preprocess_images <- function(images, base_dir, output_dir) {
  names <- vapply(images, function(x) x$name, character(1))

  images <- lapply(images, function(image) {
    resolved_style <- resolve_placeholders(image$style, base_dir)

    n <- length(resolved_style$images)
    resolved_desc <- resolve_placeholders(image$description, base_dir, n)
    prompt <- paste(
      c(
        resolved_desc$text,
        paste0("Style: ", resolved_style$text, recycle0 = TRUE)
      ),
      collapse = "\n\n"
    )
    ref_image_paths <- c(resolved_style$images, resolved_desc$images)
    ref_images <- lapply(ref_image_paths, ellmer::content_image_file)

    image$output_path <- file.path(output_dir, paste0(image$name, ".png"))
    image$prompt <- prompt
    image$ref_image_paths <- ref_image_paths
    image$ref_images <- ref_images
    image
  })

  stats::setNames(images, names)
}

generate_single_image <- function(image_spec, images) {
  image_config <- list(aspectRatio = image_spec$`aspect-ratio`)
  if (image_spec$model == "gemini-3-pro-image-preview") {
    image_config$imageSize <- image_spec$resolution
  }

  chat <- ellmer::chat_google_gemini(
    "Draw a picture based on the user's description, carefully following their
    specified style. Do not include text unless explicitly requested.",
    model = image_spec$model,
    api_args = list(
      generationConfig = list(imageConfig = image_config)
    )
  )

  # Handle builds-on chains
  builds_on <- image_spec$`builds-on`
  if (!is.null(builds_on)) {
    replay_chain(chat, builds_on, images)
  }

  chat$chat(image_spec$prompt, !!!image_spec$ref_images)
  save_generated_image(chat, image_spec$output_path)
}

replay_chain <- function(chat, builds_on, images) {
  # Find the chain of dependencies
  chain <- character()
  current <- builds_on

  while (!is.null(current)) {
    image <- images[[current]]
    if (is.null(image)) {
      cli::cli_abort("Cannot find image {.val {current}} in images list.")
    }
    chain <- c(current, chain)
    current <- image$`builds-on`
  }

  # Replay each turn in the chain
  for (name in chain) {
    image <- images[[name]]

    user_content <- list(ellmer::content_text(image$prompt))
    for (img in image$ref_images) {
      user_content <- c(user_content, list(img))
    }
    chat$add_turn("user", user_content)
    chat$add_turn(
      "assistant",
      list(ellmer::content_image_file(image$output_path))
    )
  }
}

save_generated_image <- function(chat, output_path) {
  turn <- chat$last_turn()
  image_content <- Find(
    function(x) inherits(x, "ellmer::ContentImageInline"),
    turn@contents
  )
  writeBin(
    base64enc::base64decode(image_content@data),
    output_path
  )
}
