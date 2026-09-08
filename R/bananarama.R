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

  images <- compute_output_paths(config$images, output_dir)

  # Figure out which images need to be generated
  tasks <- build_tasks(images, force = force)
  if (length(tasks) == 0) {
    return(invisible(all_output_paths(images)))
  }

  # Preprocess only the images that need generation
  for (i in seq_along(tasks)) {
    tasks[[i]]$image <- preprocess_image(tasks[[i]]$image, config$base_dir)
  }

  # Group tasks by chat config (model, seed, aspect-ratio, resolution)
  # so each group gets the correct seed
  chat_key <- function(task) {
    img <- task$image
    rlang::hash(list(img$model, img$seed, img$`aspect-ratio`, img$resolution))
  }
  task_groups <- split(tasks, vapply(tasks, chat_key, character(1)))

  cli::cli_alert("Generating {length(tasks)} image{?s} in parallel...")
  results <- vector("list", length(tasks))
  for (group in task_groups) {
    chat <- make_chat(group[[1]]$image)
    prompts <- lapply(group, function(task) {
      c(list(ellmer::ContentText(task$image$prompt)), task$image$ref_images)
    })
    group_results <- ellmer::parallel_chat(chat, prompts)

    for (i in seq_along(group)) {
      idx <- match(
        group[[i]]$output_path,
        vapply(tasks, `[[`, character(1), "output_path")
      )
      results[[idx]] <- group_results[[i]]
    }
  }

  total_cost <- 0
  for (i in seq_along(tasks)) {
    result <- results[[i]]
    output_path <- tasks[[i]]$output_path
    model <- tasks[[i]]$image$model
    label <- basename(output_path)

    if (inherits(result, "error") || is.null(result)) {
      cli::cli_alert_danger("Failed to generate {.val {label}}")
    } else if (!save_generated_image(result, output_path)) {
      cli::cli_alert_danger(
        "Failed to generate {.val {label}} (no image in response)"
      )
    } else {
      cost <- image_cost(result, model)
      total_cost <- total_cost + cost
      cli::cli_alert_success(
        "Generated {.val {label}} (${round(cost, 3)})"
      )
    }
  }

  if (total_cost > 0) {
    cli::cli_alert_info("Total cost: ${round(total_cost, 3)}")
  }

  invisible(all_output_paths(images))
}

build_tasks <- function(images, force = FALSE) {
  tasks <- list()
  n_skipped <- 0L
  for (image in images) {
    for (output_path in image$output_paths) {
      if (!force && !image$force && file.exists(output_path)) {
        n_skipped <- n_skipped + 1L
        next
      }
      tasks <- c(tasks, list(list(image = image, output_path = output_path)))
    }
  }
  if (n_skipped > 0) {
    cli::cli_alert_info("Skipping {n_skipped} image{?s} (already exist{?s})")
  }
  tasks
}

all_output_paths <- function(images) {
  unlist(lapply(images, function(image) image$output_paths))
}

compute_output_paths <- function(images, output_dir) {
  lapply(images, function(image) {
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
  })
}

preprocess_image <- function(image, base_dir) {
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
  ref_images <- lapply(ref_image_paths, get_reference_image)

  image$prompt <- prompt
  image$ref_image_paths <- ref_image_paths
  image$ref_images <- ref_images
  image
}

system_instruction <- paste(
  "Draw a picture based on the user's description, carefully following their",
  "specified style. Do not include text unless explicitly requested."
)

make_chat <- function(image_spec) {
  if (image_spec$provider == "openai") {
    return(make_chat_openai(image_spec))
  }

  image_config <- list(aspectRatio = image_spec$`aspect-ratio`)
  if (image_spec$model == "gemini-3-pro-image") {
    image_config$imageSize <- image_spec$resolution
  }

  gen_config <- list(imageConfig = image_config)
  if (!is.null(image_spec$seed)) {
    gen_config$seed <- as.integer(image_spec$seed)
  }

  ellmer::chat_google_gemini(
    system_instruction,
    model = image_spec$model,
    api_args = list(
      generationConfig = gen_config
    )
  )
}

# Prices per million tokens, by model and modality.
# Update this list when new models or pricing tiers are released.
# Models without known prices are omitted; their cost is reported as 0.
model_prices <- list(
  "gemini-3.1-flash-image-preview" = list(
    input = list(text = 0.50, image = 0.50),
    output = list(text = 3.00, image = 60.00)
  ),
  "gemini-3-pro-image" = list(
    input = list(text = 1.25, image = 1.25),
    output = list(text = 5.00, image = 60.00)
  ),
  # OpenAI gpt-image-2.5: no text output charge
  "gpt-image-2.5-flare" = list(
    input = list(text = 5.00, image = 8.00),
    output = list(text = 0, image = 30.00)
  ),
  "gpt-image-2.5-sunburst" = list(
    input = list(text = 5.00, image = 8.00),
    output = list(text = 0, image = 30.00)
  )
)

# OpenAI (Responses API via ellmer) ---------------------------------------------

# The Responses API generates images via a built-in image_generation tool
# hosted by a chat model; the gpt-image model is selected in the tool config.
openai_chat_model <- "gpt-5"

make_chat_openai <- function(image_spec) {
  # ToolBuiltIn is not yet exported by ellmer
  image_tool <- ellmer:::ToolBuiltIn(
    name = "image_gen",
    json = list(
      type = "image_generation",
      model = image_spec$model,
      size = openai_size(image_spec$`aspect-ratio`, image_spec$resolution),
      output_format = "png"
    )
  )

  chat <- ellmer::chat_openai(
    system_instruction,
    model = openai_chat_model
  )
  chat$register_tool(image_tool)
  chat
}

# Map aspect-ratio + resolution to a WIDTHxHEIGHT size. The OpenAI API
# requires dimensions divisible by 16, a ratio between 1:3 and 3:1, and
# at most 3840x2160.
openai_size <- function(aspect_ratio, resolution) {
  parts <- as.integer(strsplit(aspect_ratio, ":", fixed = TRUE)[[1]])
  ratio <- parts[1] / parts[2]

  long_side <- c("1K" = 1536, "2K" = 2048, "4K" = 4096)[[resolution]]
  if (ratio >= 1) {
    width <- long_side
    height <- long_side / ratio
  } else {
    height <- long_side
    width <- long_side * ratio
  }

  # Clamp to the API maximum of 3840x2160
  scale <- min(1, 3840 / width, 2160 / height)
  width <- floor(width * scale / 16) * 16
  height <- floor(height * scale / 16) * 16

  paste0(width, "x", height)
}

# End OpenAI -------------------------------------------------------------------

image_cost <- function(chat, model) {
  if (identical(model_registry[[model]]$provider, "openai")) {
    return(image_cost_openai(chat, model))
  }

  turn <- chat$last_turn()
  usage <- turn@json$usageMetadata

  prices <- model_prices[[model]]
  if (is.null(prices)) {
    return(0)
  }

  input_cost <- 0
  for (detail in usage$promptTokensDetails) {
    modality <- tolower(detail$modality)
    price <- prices$input[[modality]] %||% prices$input$text
    input_cost <- input_cost + detail$tokenCount * price / 1e6
  }

  output_cost <- 0
  for (detail in usage$candidatesTokensDetails) {
    modality <- tolower(detail$modality)
    price <- prices$output[[modality]] %||% prices$output$text
    output_cost <- output_cost + detail$tokenCount * price / 1e6
  }

  input_cost + output_cost
}

image_cost_openai <- function(chat, model) {
  # Chat-model (gpt-5) tokens, priced by ellmer
  cost <- as.numeric(chat$get_cost("last"))

  # Image-generation tokens are itemized in tool_usage, not usage
  prices <- model_prices[[sub("-2026-09-08$", "", model)]]
  tool_usage <- chat$last_turn()@json$tool_usage$image_gen
  if (is.null(prices) || is.null(tool_usage)) {
    return(cost)
  }

  in_d <- tool_usage$input_tokens_details
  out_d <- tool_usage$output_tokens_details
  cost +
    (in_d$text_tokens %||% 0) * prices$input$text / 1e6 +
    (in_d$image_tokens %||% 0) * prices$input$image / 1e6 +
    (out_d$text_tokens %||% 0) * prices$output$text / 1e6 +
    (out_d$image_tokens %||% 0) * prices$output$image / 1e6
}

get_reference_image <- function(path) {
  rlang::env_cache(the, path, {
    resize_reference_image(path)
    ellmer::content_image_file(path, resize = "none")
  })
}

resize_reference_image <- function(path, max_size = "512x512") {
  img <- magick::image_read(path, strip = TRUE)
  info <- magick::image_info(img)

  resized <- magick::image_resize(img, paste0(max_size, ">"))
  resized_info <- magick::image_info(resized)

  if (info$width == resized_info$width && info$height == resized_info$height) {
    return(invisible(FALSE))
  }

  magick::image_write(resized, path, format = info$format)
  cli::cli_alert_info(
    "Resizing {.path {basename(path)}} from {info$width}x{info$height} to {resized_info$width}x{resized_info$height}"
  )
  invisible(TRUE)
}

save_generated_image <- function(chat, output_path) {
  turn <- chat$last_turn()
  image_content <- Find(
    function(x) inherits(x, "ellmer::ContentImageInline"),
    turn@contents
  )
  if (is.null(image_content)) {
    text <- paste(
      vapply(
        Filter(function(x) inherits(x, "ellmer::ContentText"), turn@contents),
        function(x) x@text,
        character(1)
      ),
      collapse = "\n"
    )
    cli::cli_warn(c(
      "The model did not return an image for {.val {basename(output_path)}}.",
      i = if (nzchar(text)) "Response: {text}"
    ))
    return(invisible(FALSE))
  }
  writeBin(openssl::base64_decode(image_content@data), output_path)
  invisible(TRUE)
}
