make_chat_gemini <- function(image_spec) {
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

# Prices per million tokens, by modality.
# Update this list when new models or pricing tiers are released.
# Models without known prices are omitted; their cost is reported as 0.
gemini_prices <- list(
  "gemini-3.1-flash-image-preview" = list(
    input = list(text = 0.50, image = 0.50),
    output = list(text = 3.00, image = 60.00)
  ),
  "gemini-3-pro-image" = list(
    input = list(text = 1.25, image = 1.25),
    output = list(text = 5.00, image = 60.00)
  )
)

image_cost_gemini <- function(chat, model) {
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
