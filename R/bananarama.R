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

  # Sort images by dependencies
  images <- sort_by_dependencies(config$images)

  # Track generated images for builds-on chains
  generated <- list()
  output_paths <- character()

  for (image in images) {
    output_path <- file.path(output_dir, paste0(image$name, ".png"))
    output_paths <- c(output_paths, output_path)

    if (!force && file.exists(output_path)) {
      cli::cli_alert_info("Skipping {.val {image$name}} (already exists)")
      generated[[image$name]] <- list(
        output_path = output_path,
        prompt = image$prompt,
        ref_images = NULL
      )
      next
    }

    cli::cli_alert("Generating {.val {image$name}}...")

    result <- generate_single_image(
      image_spec = image,
      base_dir = config$base_dir,
      output_path = output_path,
      generated = generated
    )

    generated[[image$name]] <- result
    cli::cli_alert_success("Generated {.val {image$name}}")
  }

  invisible(output_paths)
}

#' Generate a single image
#'
#' @param image_spec The image specification from the config.
#' @param base_dir Directory containing reference images.
#' @param output_path Path to save the generated image.
#' @param generated List of previously generated images (for builds-on chains).
#' @return A list with the prompt and reference images used (for chain replay).
#' @noRd
generate_single_image <- function(
  image_spec,
  base_dir,
  output_path,
  generated
) {
  resolved <- resolve_placeholders(image_spec$description, base_dir)
  prompt <- build_prompt(resolved$text, image_spec$style)
  images <- lapply(resolved$images, ellmer::content_image_file)

  image_config <- list(aspectRatio = image_spec$`aspect-ratio`)
  if (image_spec$model == "gemini-3-pro-image-preview") {
    image_config$imageSize <- image_spec$resolution
  }

  chat <- ellmer::chat_google_gemini(
    model = image_spec$model,
    api_args = list(
      generationConfig = list(imageConfig = image_config)
    )
  )

  # Handle builds-on chains
  builds_on <- image_spec$`builds-on`
  if (!is.null(builds_on)) {
    replay_chain(chat, builds_on, generated)
  }

  chat$chat(prompt, !!!images)
  save_generated_image(chat, output_path)

  list(
    output_path = output_path,
    prompt = prompt,
    ref_images = resolved$images
  )
}

#' Replay a conversation chain for builds-on
#'
#' @param chat The chat session to replay into.
#' @param builds_on The name of the image this builds on.
#' @param generated List of previously generated images.
#' @noRd
replay_chain <- function(chat, builds_on, generated) {
  # Find the chain of dependencies
  chain <- character()
  current <- builds_on

  while (!is.null(current)) {
    if (!current %in% names(generated)) {
      cli::cli_abort(
        "Cannot find generated image {.val {current}} for builds-on chain."
      )
    }
    chain <- c(current, chain)

    # Find what this image built on
    prev_info <- generated[[current]]
    # We need to track the builds-on info - for now, assume linear chain
    current <- NULL
  }

  # Replay each turn in the chain
  for (name in chain) {
    info <- generated[[name]]
    # Replay the original prompt and response
    user_content <- list(ellmer::content_text(info$prompt))
    if (length(info$ref_images) > 0) {
      for (img in info$ref_images) {
        user_content <- c(user_content, list(ellmer::content_image_file(img)))
      }
    }
    chat$add_turn("user", user_content)
    chat$add_turn(
      "assistant",
      list(ellmer::content_image_file(info$output_path))
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
