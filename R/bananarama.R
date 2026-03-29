#' Generate presentation images from a YAML configuration
#'
#' @param path Path to a YAML configuration file or a directory containing
#'   `bananarama.yaml`. Defaults to `"bananarama.yaml"` in the current directory.
#' @param output_dir Directory to save generated images, relative to the
#'   YAML configuration file (or an absolute path). Defaults to the
#'   `output-dir` field in the YAML file, or a directory with the same name
#'   as the YAML file (e.g. `bananarama.yaml` outputs to `bananarama/`).
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

  default_dir <- tools::file_path_sans_ext(basename(config_path))
  output_dir <- output_dir %||% config$output_dir %||% default_dir
  if (!startsWith(output_dir, "/")) {
    output_dir <- file.path(config$base_dir, output_dir)
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  images <- preprocess_images(config$images, config$base_dir, output_dir)

  # Figure out which images need to be generated
  tasks <- build_tasks(images, force = force)
  if (length(tasks) == 0) {
    return(invisible(all_output_paths(images)))
  }

  # Generate all images in parallel
  chat <- make_chat(tasks[[1]]$image)
  prompts <- lapply(tasks, function(task) {
    c(list(ellmer::ContentText(task$image$prompt)), task$image$ref_images)
  })

  cli::cli_alert("Generating {length(tasks)} image{?s} in parallel...")
  results <- ellmer::parallel_chat(chat, prompts)

  for (i in seq_along(tasks)) {
    result <- results[[i]]
    output_path <- tasks[[i]]$output_path
    label <- basename(output_path)

    if (inherits(result, "error") || is.null(result)) {
      cli::cli_alert_danger("Failed to generate {.val {label}}")
    } else {
      save_generated_image(result, output_path)
      cli::cli_alert_success("Generated {.val {label}}")
    }
  }

  invisible(all_output_paths(images))
}

build_tasks <- function(images, force = FALSE) {
  tasks <- list()
  for (image in images) {
    for (output_path in image$output_paths) {
      if (!force && !image$force && file.exists(output_path)) {
        cli::cli_alert_info(
          "Skipping {.val {basename(output_path)}} (already exists)"
        )
        next
      }
      tasks <- c(tasks, list(list(image = image, output_path = output_path)))
    }
  }
  tasks
}

all_output_paths <- function(images) {
  unlist(lapply(images, function(image) image$output_paths))
}

preprocess_images <- function(images, base_dir, output_dir) {
  lapply(images, preprocess_image, base_dir = base_dir, output_dir = output_dir)
}

preprocess_image <- function(image, base_dir, output_dir) {
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

  image$prompt <- prompt
  image$ref_image_paths <- ref_image_paths
  image$ref_images <- ref_images

  n <- image[["n"]] %||% 1L
  if (n > 1L) {
    suffixed_names <- paste0(image$name, "-", seq_len(n))
  } else {
    suffixed_names <- image$name
  }
  image$output_paths <- file.path(
    output_dir,
    paste0(suffixed_names, ".png")
  )
  image
}

make_chat <- function(image_spec) {
  image_config <- list(aspectRatio = image_spec$`aspect-ratio`)
  if (image_spec$model == "gemini-3-pro-image-preview") {
    image_config$imageSize <- image_spec$resolution
  }

  ellmer::chat_google_gemini(
    "Draw a picture based on the user's description, carefully following their
    specified style. Do not include text unless explicitly requested.",
    model = image_spec$model,
    api_args = list(
      generationConfig = list(imageConfig = image_config)
    )
  )
}

save_generated_image <- function(chat, output_path) {
  turn <- chat$last_turn()
  image_content <- Find(
    function(x) inherits(x, "ellmer::ContentImageInline"),
    turn@contents
  )
  writeBin(openssl::base64_decode(image_content@data), output_path)
}
