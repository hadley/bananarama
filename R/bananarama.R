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

  tasks <- build_tasks(config$images, output_dir, force = force)
  if (length(tasks$pending) == 0) {
    return(invisible(tasks$paths))
  }

  # Tasks form a dependency graph via parent_path: a task is ready once its
  # parent's output file exists, whether cached on disk or freshly generated.
  total_cost <- 0
  pending <- tasks$pending
  while (length(pending) > 0) {
    ready <- vapply(pending, task_ready, logical(1))
    if (!any(ready)) {
      missing <- unique(vapply(pending, `[[`, character(1), "parent_path"))
      cli::cli_abort(c(
        "Cannot generate {length(pending)} image{?s} because their parent images are missing:",
        x = "{.path {missing}}"
      ))
    }
    total_cost <- total_cost + run_wave(pending[ready], config$base_dir)
    pending <- pending[!ready]
  }

  if (total_cost > 0) {
    cli::cli_alert_info("Total cost: ${round(total_cost, 3)}")
  }

  invisible(tasks$paths)
}

task_ready <- function(task) {
  is.null(task$parent_path) || file.exists(task$parent_path)
}

run_wave <- function(wave, base_dir) {
  for (i in seq_along(wave)) {
    wave[[i]]$image <- preprocess_image(
      wave[[i]]$image,
      base_dir,
      wave[[i]]$parent_path
    )
  }

  # Group tasks by chat config (model, seed, aspect-ratio, resolution)
  # so each group gets the correct seed
  chat_key <- function(task) {
    img <- task$image
    rlang::hash(list(img$model, img$seed, img$`aspect-ratio`, img$resolution))
  }
  task_groups <- split(wave, vapply(wave, chat_key, character(1)))

  cli::cli_alert("Generating {length(wave)} image{?s} in parallel...")
  results <- vector("list", length(wave))
  for (group in task_groups) {
    chat <- make_chat(group[[1]]$image)
    prompts <- lapply(group, function(task) {
      c(list(ellmer::ContentText(task$image$prompt)), task$image$ref_images)
    })
    group_results <- ellmer::parallel_chat(chat, prompts)

    for (i in seq_along(group)) {
      idx <- match(
        group[[i]]$output_path,
        vapply(wave, `[[`, character(1), "output_path")
      )
      results[[idx]] <- group_results[[i]]
    }
  }

  total_cost <- 0
  for (i in seq_along(wave)) {
    result <- results[[i]]
    output_path <- wave[[i]]$output_path
    model <- wave[[i]]$image$model
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

  total_cost
}

# Flatten images (plain or sequences) into a list of tasks, one per output
# file. Each task has an image spec, an output_path, and a parent_path
# (NULL unless this is a sequence step building on a previous step's file).
build_tasks <- function(images, output_dir, force = FALSE) {
  all <- list()
  for (image in images) {
    if (!is.null(image$sequence)) {
      all <- c(all, sequence_tasks(image, output_dir))
    } else {
      n <- image$n %||% 1L
      nms <- if (n > 1L) paste0(image$name, "-", seq_len(n)) else image$name
      for (nm in nms) {
        all <- c(
          all,
          list(list(
            image = image,
            output_path = file.path(output_dir, paste0(nm, ".png")),
            parent_path = NULL,
            force = image$force %||% FALSE
          ))
        )
      }
    }
  }

  paths <- vapply(all, `[[`, character(1), "output_path")
  skip <- !force &
    !vapply(all, function(task) isTRUE(task$force), logical(1)) &
    file.exists(paths)
  if (any(skip)) {
    cli::cli_alert_info("Skipping {sum(skip)} image{?s} (already exist{?s})")
  }

  list(pending = all[!skip], paths = paths)
}

# Walk a sequence tree, emitting one task per step (with a description) per
# iteration. The image's top-level sequence is a chain: each step builds on
# the previous step's output. A step's nested sequence is a set of branches:
# every child builds on the step's own output. parent_paths tracks the files
# to build on, one per iteration, so iteration i always builds on iteration i
# of the parent.
sequence_tasks <- function(image, output_dir) {
  tasks <- list()
  n <- image$n %||% 1L

  # Emit tasks for a step; returns the paths later steps should build on
  # (the step's own paths, or unchanged if the step is a pure branch point).
  emit <- function(step, parent_paths) {
    if (is.null(step$description)) {
      return(parent_paths)
    }
    paths <- character(n)
    for (i in seq_len(n)) {
      nm <- if (n > 1L) paste0(step$full_name, "-", i) else step$full_name
      paths[[i]] <- file.path(output_dir, paste0(nm, ".png"))

      spec <- step
      spec$sequence <- NULL
      spec$resolution <- image$resolution
      tasks[[length(tasks) + 1L]] <<- list(
        image = spec,
        output_path = paths[[i]],
        parent_path = if (is.null(parent_paths)) NULL else parent_paths[[i]],
        force = image$force %||% FALSE
      )
    }
    paths
  }

  # Nested sequences branch: every child builds on the same parent paths.
  walk_branches <- function(steps, parent_paths) {
    for (step in steps) {
      paths <- emit(step, parent_paths)
      if (!is.null(step$sequence)) {
        walk_branches(step$sequence, paths)
      }
    }
  }

  # The top-level sequence chains: each step builds on the previous step.
  parent_paths <- NULL
  for (step in image$sequence) {
    paths <- emit(step, parent_paths)
    if (!is.null(step$sequence)) {
      walk_branches(step$sequence, paths)
    }
    parent_paths <- paths
  }
  tasks
}

preprocess_image <- function(image, base_dir, parent_path = NULL) {
  # A parent image (from the previous sequence step) is passed as the first
  # reference image; use it as-is rather than resizing the cached output.
  parent_images <- list()
  start_index <- 0L
  if (!is.null(parent_path)) {
    parent_images <- list(ellmer::content_image_file(
      parent_path,
      resize = "none"
    ))
    start_index <- 1L
  }

  resolved_style <- resolve_placeholders(image$style, base_dir, start_index)

  n <- start_index + length(resolved_style$images)
  resolved_desc <- resolve_placeholders(image$description, base_dir, n)
  prompt <- paste(
    c(
      if (!is.null(parent_path)) "Modify the first image as follows:",
      resolved_desc$text,
      paste0("Style: ", resolved_style$text, recycle0 = TRUE)
    ),
    collapse = "\n\n"
  )
  ref_image_paths <- c(
    parent_path %||% character(),
    resolved_style$images,
    resolved_desc$images
  )
  ref_images <- c(
    parent_images,
    lapply(c(resolved_style$images, resolved_desc$images), get_reference_image)
  )

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
    make_chat_openai(image_spec)
  } else {
    make_chat_gemini(image_spec)
  }
}

image_cost <- function(chat, model) {
  if (identical(model_registry[[model]]$provider, "openai")) {
    image_cost_openai(chat, model)
  } else {
    image_cost_gemini(chat, model)
  }
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
