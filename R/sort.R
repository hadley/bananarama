sort_by_dependencies <- function(images) {
  if (length(images) == 0) {
    return(images)
  }

  # Build a lookup of image names to indices
  names <- vapply(images, function(x) x$name, character(1))
  name_to_idx <- setNames(seq_along(images), names)

  # Build adjacency list: for each image, which images depend on it?
  # Also track in-degree (number of dependencies) for each image
  in_degree <- rep(0L, length(images))
  dependents <- vector("list", length(images))

  for (i in seq_along(images)) {
    dep <- images[[i]]$`builds-on`
    if (!is.null(dep)) {
      if (!dep %in% names) {
        cli::cli_abort(c(
          "Image {.val {images[[i]]$name}} depends on {.val {dep}}, which does not exist.",
          i = "Available images: {.val {names}}."
        ))
      }
      dep_idx <- name_to_idx[[dep]]
      in_degree[[i]] <- 1L
      dependents[[dep_idx]] <- c(dependents[[dep_idx]], i)
    }
  }

  # Kahn's algorithm for topological sort
  detect_cycle(images, name_to_idx, in_degree)

  result <- vector("list", length(images))
  result_idx <- 1L

  # Start with nodes that have no dependencies
  queue <- which(in_degree == 0)

  while (length(queue) > 0) {
    # Take first from queue
    current <- queue[[1]]
    queue <- queue[-1]

    result[[result_idx]] <- images[[current]]
    result_idx <- result_idx + 1L

    # Reduce in-degree for dependents
    for (dep in dependents[[current]]) {
      in_degree[[dep]] <- in_degree[[dep]] - 1L
      if (in_degree[[dep]] == 0) {
        queue <- c(queue, dep)
      }
    }
  }

  result
}

detect_cycle <- function(images, name_to_idx, in_degree) {
  # If there's a cycle, we won't be able to process all images
  # We can detect this by doing a DFS and looking for back edges

  names <- vapply(
    images,
    function(x) x$name,
    character(
      1
    )
  )
  visited <- rep(FALSE, length(images))
  rec_stack <- rep(FALSE, length(images))

  find_cycle <- function(i, path) {
    visited[[i]] <<- TRUE
    rec_stack[[i]] <<- TRUE

    dep_name <- images[[i]]$`builds-on`
    if (!is.null(dep_name)) {
      dep_idx <- name_to_idx[[dep_name]]
      if (!visited[[dep_idx]]) {
        result <- find_cycle(dep_idx, c(path, names[[i]]))
        if (!is.null(result)) {
          return(result)
        }
      } else if (rec_stack[[dep_idx]]) {
        # Found a cycle
        cycle_start <- which(path == names[[dep_idx]])
        if (length(cycle_start) > 0) {
          return(c(
            path[cycle_start:length(path)],
            names[[i]],
            names[[dep_idx]]
          ))
        }
        return(c(names[[dep_idx]], names[[i]], names[[dep_idx]]))
      }
    }

    rec_stack[[i]] <<- FALSE
    NULL
  }

  for (i in seq_along(images)) {
    if (!visited[[i]]) {
      cycle <- find_cycle(i, character())
      if (!is.null(cycle)) {
        cli::cli_abort(c(
          "Circular dependency detected.",
          i = "Cycle: {.val {paste(cycle, collapse = ' -> ')}}."
        ))
      }
    }
  }
}
