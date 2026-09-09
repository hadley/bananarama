#' @keywords internal
"_PACKAGE"

the <- new.env(parent = emptyenv())

# Silence R CMD check warning
unused <- function() {
  # Use via ellmer::content_image()
  magick::image_resize()
}

# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL

## usethis namespace: start
#' @importFrom rlang %||%
## usethis namespace: end

## mockable bindings: start
## mockable bindings: end
NULL
