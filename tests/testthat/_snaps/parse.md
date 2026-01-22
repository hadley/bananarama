# parse_image errors on invalid aspect-ratio

    Code
      parse_image(img, defaults)
    Condition
      Error in `check_aspect_ratio()`:
      ! Image "test" has invalid aspect-ratio "5:3".
      i Must be one of "1:1", "2:3", "3:2", "3:4", "4:3", "4:5", "5:4", "9:16", "16:9", or "21:9".

# parse_image errors on invalid resolution

    Code
      parse_image(img, defaults)
    Condition
      Error in `check_resolution()`:
      ! Image "test" has invalid resolution "8K".
      i Must be one of "1K", "2K", or "4K".

