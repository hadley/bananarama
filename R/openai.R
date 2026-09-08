# OpenAI (Responses API via ellmer)

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

# Prices per million tokens, by modality; no text output charge.
openai_prices <- list(
  "gpt-image-2.5-flare" = list(
    input = list(text = 5.00, image = 8.00),
    output = list(text = 0, image = 30.00)
  ),
  "gpt-image-2.5-sunburst" = list(
    input = list(text = 5.00, image = 8.00),
    output = list(text = 0, image = 30.00)
  )
)

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
