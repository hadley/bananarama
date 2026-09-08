test_that("compute_output_paths sets output_paths", {
  output_dir <- "/tmp/output"

  images <- list(
    list(name = "img1", n = 1L),
    list(name = "img2", n = 3L)
  )

  result <- compute_output_paths(images, output_dir)

  expect_equal(result[[1]]$output_paths, file.path(output_dir, "img1.png"))
  expect_equal(
    result[[2]]$output_paths,
    file.path(output_dir, c("img2-1.png", "img2-2.png", "img2-3.png"))
  )
})

test_that("preprocess_image adds prompt, paths, and ref_images", {
  tmp <- withr::local_tempdir()
  png::writePNG(array(1, c(1, 1, 3)), file.path(tmp, "cat.png"))

  img1 <- list(
    name = "img1",
    description = "A [cat] sitting",
    style = "Watercolor"
  )
  result1 <- preprocess_image(img1, tmp)
  expect_equal(
    result1$prompt,
    "A cat (shown in image 1) sitting\n\nStyle: Watercolor"
  )
  expect_equal(result1$ref_image_paths, file.path(tmp, "cat.png"))
  expect_length(result1$ref_images, 1)

  img2 <- list(
    name = "img2",
    description = "A simple scene",
    style = NULL
  )
  result2 <- preprocess_image(img2, tmp)
  expect_equal(result2$prompt, "A simple scene")
  expect_equal(result2$ref_image_paths, character())
  expect_length(result2$ref_images, 0)
})

test_that("preprocess_image handles placeholders in style", {
  tmp <- withr::local_tempdir()
  png::writePNG(array(1, c(1, 1, 3)), file.path(tmp, "monet.png"))
  png::writePNG(array(1, c(1, 1, 3)), file.path(tmp, "cat.png"))

  img <- list(
    name = "img1",
    description = "A [cat] sitting",
    style = "In the style of [monet]"
  )

  result <- preprocess_image(img, tmp)

  expect_equal(
    result$prompt,
    "A cat (shown in image 2) sitting\n\nStyle: In the style of monet (shown in image 1)"
  )
  expect_equal(
    result$ref_image_paths,
    c(file.path(tmp, "monet.png"), file.path(tmp, "cat.png"))
  )
  expect_length(result$ref_images, 2)
})

test_that("provider price tables cover known models", {
  expect_named(
    gemini_prices,
    c("gemini-3.1-flash-image-preview", "gemini-3-pro-image")
  )
  expect_named(
    openai_prices,
    c("gpt-image-2.5-flare", "gpt-image-2.5-sunburst")
  )
  for (prices in c(gemini_prices, openai_prices)) {
    expect_named(prices, c("input", "output"))
    expect_true("text" %in% names(prices$input))
    expect_true("image" %in% names(prices$output))
  }
})

test_that("openai_size maps aspect ratio and resolution to pixels", {
  expect_equal(openai_size("16:9", "1K"), "1536x864")
  expect_equal(openai_size("1:1", "1K"), "1536x1536")
  expect_equal(openai_size("3:2", "2K"), "2048x1360")
  expect_equal(openai_size("9:16", "1K"), "864x1536")
  # 4K is clamped to the API maximum of 3840x2160
  expect_equal(openai_size("16:9", "4K"), "3840x2160")
})

test_that("openai_size dimensions are divisible by 16", {
  ratios <- c(
    "1:1",
    "2:3",
    "3:2",
    "3:4",
    "4:3",
    "4:5",
    "5:4",
    "9:16",
    "16:9",
    "21:9"
  )
  for (ratio in ratios) {
    dims <- as.integer(strsplit(openai_size(ratio, "2K"), "x")[[1]])
    expect_true(all(dims %% 16 == 0), label = ratio)
  }
})

test_that("make_chat_openai registers an image_generation tool", {
  spec <- list(
    model = "gpt-image-2.5-flare",
    provider = "openai",
    `aspect-ratio` = "16:9",
    resolution = "1K"
  )
  chat <- make_chat_openai(spec)
  tools <- chat$get_tools()
  expect_length(tools, 1)
  expect_equal(tools[[1]]@json$type, "image_generation")
  expect_equal(tools[[1]]@json$model, "gpt-image-2.5-flare")
  expect_equal(tools[[1]]@json$size, "1536x864")
})

test_that("compute_output_paths expands n into multiple output_paths", {
  output_dir <- "/tmp/output"

  images <- list(
    list(name = "bicycle", n = 3L),
    list(name = "car", n = 1L)
  )

  result <- compute_output_paths(images, output_dir)

  expect_equal(
    result[[1]]$output_paths,
    file.path(output_dir, c("bicycle-1.png", "bicycle-2.png", "bicycle-3.png"))
  )
  expect_equal(result[[2]]$output_paths, file.path(output_dir, "car.png"))
})
