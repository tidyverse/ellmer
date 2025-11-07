# Encode images for chat input

These functions are used to prepare image URLs and files for input to
the chatbot. The `content_image_url()` function is used to provide a URL
to an image, while `content_image_file()` is used to provide the image
data itself.

## Usage

``` r
content_image_url(url, detail = c("auto", "low", "high"))

content_image_file(path, content_type = "auto", resize = "low")

content_image_plot(width = 768, height = 768)
```

## Arguments

- url:

  The URL of the image to include in the chat input. Can be a `data:`
  URL or a regular URL. Valid image types are PNG, JPEG, WebP, and
  non-animated GIF.

- detail:

  The [detail
  setting](https://platform.openai.com/docs/guides/images/image-input-requirements)
  for this image. Can be `"auto"`, `"low"`, or `"high"`.

- path:

  The path to the image file to include in the chat input. Valid file
  extensions are `.png`, `.jpeg`, `.jpg`, `.webp`, and (non-animated)
  `.gif`.

- content_type:

  The content type of the image (e.g. `image/png`). If `"auto"`, the
  content type is inferred from the file extension.

- resize:

  If `"low"`, resize images to fit within 512x512. If `"high"`, resize
  to fit within 2000x768 or 768x2000. (See the [OpenAI
  docs](https://platform.openai.com/docs/guides/images/image-input-requirements)
  for more on why these specific sizes are used.) If `"none"`, do not
  resize.

  You can also pass a custom string to resize the image to a specific
  size, e.g. `"200x200"` to resize to 200x200 pixels while preserving
  aspect ratio. Append `>` to resize only if the image is larger than
  the specified size, and `!` to ignore aspect ratio (e.g.
  `"300x200>!"`).

  All values other than `none` require the `magick` package.

- width, height:

  Width and height in pixels.

## Value

An input object suitable for including in the `...` parameter of the
[`chat()`](https://ellmer.tidyverse.org/dev/reference/chat-any.md),
`stream()`, `chat_async()`, or `stream_async()` methods.

## Examples

``` r
if (FALSE) { # \dontrun{
chat <- chat_openai()
chat$chat(
  "What do you see in these images?",
  content_image_url("https://www.r-project.org/Rlogo.png"),
  content_image_file(system.file("httr2.png", package = "ellmer"))
)

plot(waiting ~ eruptions, data = faithful)
chat <- chat_openai()
chat$chat(
  "Describe this plot in one paragraph, as suitable for inclusion in
   alt-text. You should briefly describe the plot type, the axes, and
   2-5 major visual patterns.",
   content_image_plot()
)
} # }
```
