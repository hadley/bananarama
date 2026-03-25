test_that("preprocess_images adds prompt, paths, and ref_images", {
  tmp <- withr::local_tempdir()
  # Create a valid 1x1 PNG image
  png::writePNG(array(1, c(1, 1, 3)), file.path(tmp, "cat.png"))
  output_dir <- file.path(tmp, "output")

  images <- list(
    list(
      name = "img1",
      description = "A [cat] sitting",
      style = "Watercolor"
    ),
    list(
      name = "img2",
      description = "A simple scene",
      style = NULL
    )
  )

  result <- preprocess_images(images, tmp, output_dir)

  expect_equal(
    result[[1]]$prompt,
    "A cat (shown in image 1) sitting\n\nStyle: Watercolor"
  )
  expect_equal(result[[1]]$output_paths, file.path(output_dir, "img1.png"))
  expect_equal(result[[1]]$ref_image_paths, file.path(tmp, "cat.png"))
  expect_length(result[[1]]$ref_images, 1)

  expect_equal(result[[2]]$prompt, "A simple scene")
  expect_equal(result[[2]]$output_paths, file.path(output_dir, "img2.png"))
  expect_equal(result[[2]]$ref_image_paths, character())
  expect_length(result[[2]]$ref_images, 0)
})

test_that("preprocess_images handles placeholders in style", {
  tmp <- withr::local_tempdir()
  png::writePNG(array(1, c(1, 1, 3)), file.path(tmp, "monet.png"))
  png::writePNG(array(1, c(1, 1, 3)), file.path(tmp, "cat.png"))
  output_dir <- file.path(tmp, "output")

  images <- list(list(
    name = "img1",
    description = "A [cat] sitting",
    style = "In the style of [monet]"
  ))

  result <- preprocess_images(images, tmp, output_dir)

  expect_equal(
    result[[1]]$prompt,
    "A cat (shown in image 2) sitting\n\nStyle: In the style of monet (shown in image 1)"
  )
  expect_equal(
    result[[1]]$ref_image_paths,
    c(file.path(tmp, "monet.png"), file.path(tmp, "cat.png"))
  )
  expect_length(result[[1]]$ref_images, 2)
})

test_that("preprocess_images expands n into multiple output_paths", {
  tmp <- withr::local_tempdir()
  output_dir <- file.path(tmp, "output")

  images <- list(
    list(name = "bicycle", description = "A bicycle", style = NULL, n = 3L),
    list(name = "car", description = "A car", style = NULL, n = 1L)
  )

  result <- preprocess_images(images, tmp, output_dir)

  expect_equal(
    result[[1]]$output_paths,
    file.path(output_dir, c("bicycle-1.png", "bicycle-2.png", "bicycle-3.png"))
  )
  expect_equal(result[[2]]$output_paths, file.path(output_dir, "car.png"))
})
