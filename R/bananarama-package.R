#' @keywords internal
"_PACKAGE"

the <- new.env(parent = emptyenv())

# Silence R CMD check warning
unused <- function() {
  # Use via ellmer::content_image()
  magick::image_resize()
}

## usethis namespace: start
#' @importFrom rlang %||%
## usethis namespace: end

## mockable bindings: start
## mockable bindings: end
NULL
