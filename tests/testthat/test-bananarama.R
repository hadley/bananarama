test_that("build_tasks expands n into multiple tasks", {
  output_dir <- withr::local_tempdir()

  images <- list(
    list(name = "img1", n = 1L),
    list(name = "img2", n = 3L)
  )

  result <- build_tasks(images, output_dir)

  expect_equal(
    result$paths,
    file.path(
      output_dir,
      c("img1.png", "img2-1.png", "img2-2.png", "img2-3.png")
    )
  )
  expect_length(result$pending, 4)
  expect_true(all(vapply(
    result$pending,
    function(task) is.null(task$parent_path),
    logical(1)
  )))
})

test_that("build_tasks skips existing files unless forced", {
  output_dir <- withr::local_tempdir()
  file.create(file.path(output_dir, "img1.png"))

  images <- list(list(name = "img1", n = 1L), list(name = "img2", n = 1L))

  result <- build_tasks(images, output_dir)
  expect_length(result$pending, 1)
  expect_equal(basename(result$pending[[1]]$output_path), "img2.png")

  result <- build_tasks(images, output_dir, force = TRUE)
  expect_length(result$pending, 2)

  images[[1]]$force <- TRUE
  result <- build_tasks(images, output_dir)
  expect_length(result$pending, 2)
})

test_that("sequence_tasks creates one task per step with parent wiring", {
  output_dir <- withr::local_tempdir()

  image <- list(
    name = "seq",
    n = 1L,
    resolution = "1K",
    sequence = list(
      list(
        name = "base",
        full_name = "seq-base",
        description = "A scene",
        sequence = list(
          list(
            name = "day",
            full_name = "seq-base-day",
            description = "Make it day",
            sequence = NULL
          ),
          list(
            name = "night",
            full_name = "seq-base-night",
            description = NULL,
            sequence = list(
              list(
                name = "stars",
                full_name = "seq-base-night-stars",
                description = "Add stars",
                sequence = NULL
              )
            )
          )
        )
      )
    )
  )

  tasks <- sequence_tasks(image, output_dir)

  expect_equal(
    vapply(tasks, `[[`, character(1), "output_path"),
    file.path(
      output_dir,
      c("seq-base.png", "seq-base-day.png", "seq-base-night-stars.png")
    )
  )
  # Root step has no parent; branches point at the nearest imaged ancestor
  expect_null(tasks[[1]]$parent_path)
  expect_equal(tasks[[2]]$parent_path, tasks[[1]]$output_path)
  # Pure branch point (night) produces no file; its child builds on base
  expect_equal(tasks[[3]]$parent_path, tasks[[1]]$output_path)
})

test_that("nested sequences branch in parallel off their step", {
  output_dir <- withr::local_tempdir()

  image <- list(
    name = "seq",
    n = 1L,
    resolution = "1K",
    sequence = list(
      list(
        name = "a",
        full_name = "seq-a",
        description = "a",
        sequence = list(
          list(
            name = "b",
            full_name = "seq-a-b",
            description = "b",
            sequence = NULL
          ),
          list(
            name = "c",
            full_name = "seq-a-c",
            description = "c",
            sequence = NULL
          )
        )
      )
    )
  )

  tasks <- sequence_tasks(image, output_dir)

  # Both branches build on a, not on each other
  expect_equal(tasks[[2]]$parent_path, tasks[[1]]$output_path)
  expect_equal(tasks[[3]]$parent_path, tasks[[1]]$output_path)
})

test_that("sequence_tasks chains siblings sequentially", {
  output_dir <- withr::local_tempdir()

  image <- list(
    name = "seq",
    n = 1L,
    resolution = "1K",
    sequence = list(
      list(name = "a", full_name = "seq-a", description = "a", sequence = NULL),
      list(name = "b", full_name = "seq-b", description = "b", sequence = NULL),
      list(name = "c", full_name = "seq-c", description = "c", sequence = NULL)
    )
  )

  tasks <- sequence_tasks(image, output_dir)

  expect_null(tasks[[1]]$parent_path)
  expect_equal(tasks[[2]]$parent_path, tasks[[1]]$output_path)
  expect_equal(tasks[[3]]$parent_path, tasks[[2]]$output_path)
})

test_that("sequence_tasks expands n into parallel iterations", {
  output_dir <- withr::local_tempdir()

  image <- list(
    name = "seq",
    n = 2L,
    resolution = "1K",
    sequence = list(
      list(
        name = "a",
        full_name = "seq-a",
        description = "Step a",
        sequence = list(
          list(
            name = "b",
            full_name = "seq-a-b",
            description = "Step b",
            sequence = NULL
          )
        )
      )
    )
  )

  tasks <- sequence_tasks(image, output_dir)

  expect_equal(
    vapply(tasks, `[[`, character(1), "output_path"),
    file.path(
      output_dir,
      c("seq-a-1.png", "seq-a-2.png", "seq-a-b-1.png", "seq-a-b-2.png")
    )
  )
  # Iteration i of step b builds on iteration i of step a
  expect_equal(tasks[[3]]$parent_path, tasks[[1]]$output_path)
  expect_equal(tasks[[4]]$parent_path, tasks[[2]]$output_path)
})

test_that("task_ready depends on parent file existence", {
  tmp <- withr::local_tempdir()
  parent <- file.path(tmp, "parent.png")

  root <- list(parent_path = NULL)
  expect_true(task_ready(root))

  child <- list(parent_path = parent)
  expect_false(task_ready(child))

  file.create(parent)
  expect_true(task_ready(child))
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

test_that("preprocess_image includes parent image for sequence steps", {
  tmp <- withr::local_tempdir()
  parent <- file.path(tmp, "parent.png")
  png::writePNG(array(1, c(1, 1, 3)), parent)
  png::writePNG(array(1, c(1, 1, 3)), file.path(tmp, "cat.png"))

  img <- list(
    name = "seq-step",
    description = "Add a [cat]",
    style = "Watercolor"
  )
  result <- preprocess_image(img, tmp, parent_path = parent)

  expect_equal(
    result$prompt,
    "Modify the first image as follows:\n\nAdd a cat (shown in image 2)\n\nStyle: Watercolor"
  )
  expect_equal(
    result$ref_image_paths,
    c(parent, file.path(tmp, "cat.png"))
  )
  expect_length(result$ref_images, 2)
})
