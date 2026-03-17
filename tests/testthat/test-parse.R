test_that("parse_defaults handles missing defaults", {
  result <- parse_defaults(NULL)
  expect_equal(result$model, "gemini-3.1-flash-image-preview")
  expect_null(result$style)
  expect_equal(result$`aspect-ratio`, "1:1")
  expect_equal(result$resolution, "1K")
})

test_that("parse_defaults extracts values from named list", {
  defaults <- list(
    style = "chunky gouache",
    `aspect-ratio` = "16:9",
    resolution = "2K",
    model = "gemini-3-pro-image-preview"
  )
  result <- parse_defaults(defaults)
  expect_equal(result$model, "gemini-3-pro-image-preview")
  expect_equal(result$style, "chunky gouache")
  expect_equal(result$`aspect-ratio`, "16:9")
  expect_equal(result$resolution, "2K")
})

test_that("parse_image errors on missing name", {
  expect_error(parse_image(list(description = "A picture"), list()), "name")
})

test_that("parse_image errors on missing description", {
  defaults <- parse_defaults(NULL)
  expect_error(parse_image(list(name = "test"), defaults), "description")
})

test_that("parse_image uses description from defaults", {
  defaults <- parse_defaults(list(description = "default desc"))
  result <- parse_image(list(name = "test", style = "watercolor"), defaults)
  expect_equal(result$description, "default desc")

  result <- parse_image(
    list(name = "test", description = "custom desc"),
    defaults
  )
  expect_equal(result$description, "custom desc")
})

test_that("parse_image merges defaults with overrides", {
  defaults <- list(
    model = "gemini-2.5-flash-image",
    style = "default style",
    `aspect-ratio` = "1:1",
    resolution = "1K"
  )

  result <- parse_image(list(name = "img1", description = "desc1"), defaults)
  expect_equal(result$model, "gemini-2.5-flash-image")
  expect_equal(result$style, "default style")
  expect_equal(result$`aspect-ratio`, "1:1")

  result <- parse_image(
    list(name = "img2", description = "desc2", `aspect-ratio` = "16:9"),
    defaults
  )
  expect_equal(result$style, "default style")
  expect_equal(result$`aspect-ratio`, "16:9")

  result <- parse_image(
    list(
      name = "img3",
      description = "desc3",
      model = "gemini-3-pro-image-preview"
    ),
    defaults
  )
  expect_equal(result$model, "gemini-3-pro-image-preview")
})

test_that("parse_image errors on invalid aspect-ratio", {
  defaults <- parse_defaults(NULL)
  img <- list(name = "test", description = "desc", `aspect-ratio` = "5:3")
  expect_snapshot(parse_image(img, defaults), error = TRUE)
})

test_that("parse_image errors on invalid resolution", {
  defaults <- parse_defaults(NULL)
  img <- list(name = "test", description = "desc", resolution = "8K")
  expect_snapshot(parse_image(img, defaults), error = TRUE)
})

test_that("resolve_placeholders returns unchanged text without placeholders", {
  result <- resolve_placeholders("A simple description", tempdir())
  expect_equal(result$text, "A simple description")
  expect_equal(result$images, character())
})

test_that("resolve_placeholders replaces placeholders with hybrid references", {
  # Create temp images
  tmp <- withr::local_tempdir()
  file.create(file.path(tmp, "hadley.jpg"))
  file.create(file.path(tmp, "robot.png"))

  result <- resolve_placeholders("Draw [hadley] with [robot] in a garden", tmp)

  expect_equal(
    result$text,
    "Draw hadley (shown in image 1) with robot (shown in image 2) in a garden"
  )
  expect_equal(
    result$images,
    c(file.path(tmp, "hadley.jpg"), file.path(tmp, "robot.png"))
  )
})

test_that("resolve_placeholders handles repeated placeholders", {
  tmp <- withr::local_tempdir()
  file.create(file.path(tmp, "robot.png"))

  result <- resolve_placeholders("A [robot] meets another [robot]", tmp)

  expect_equal(
    result$text,
    "A robot (shown in image 1) meets another robot (shown in image 2)"
  )
  expect_length(result$images, 2)
})

test_that("find_image_file finds png files", {
  tmp <- withr::local_tempdir()
  file.create(file.path(tmp, "test.png"))

  result <- find_image_file("test", tmp)
  expect_equal(result, file.path(tmp, "test.png"))
})

test_that("find_image_file finds jpg files", {
  tmp <- withr::local_tempdir()
  file.create(file.path(tmp, "test.jpg"))

  result <- find_image_file("test", tmp)
  expect_equal(result, file.path(tmp, "test.jpg"))
})

test_that("parse_image_config uses output-dir from defaults", {
  tmp <- withr::local_tempdir()
  yaml::write_yaml(
    list(
      defaults = list(`output-dir` = "imgs"),
      images = list(list(name = "test", description = "desc"))
    ),
    file.path(tmp, "bananarama.yaml")
  )

  result <- parse_image_config(file.path(tmp, "bananarama.yaml"))
  expect_equal(result$output_dir, file.path(tmp, "imgs"))
})

test_that("parse_image_config returns NULL output_dir when not specified", {
  tmp <- withr::local_tempdir()
  yaml::write_yaml(
    list(images = list(list(name = "test", description = "desc"))),
    file.path(tmp, "bananarama.yaml")
  )

  result <- parse_image_config(file.path(tmp, "bananarama.yaml"))
  expect_null(result$output_dir)
})

test_that("find_image_file errors on missing file", {
  tmp <- withr::local_tempdir()
  expect_error(
    find_image_file("nonexistent", tmp),
    "Cannot find reference image"
  )
})
