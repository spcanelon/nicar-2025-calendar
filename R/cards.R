card <- function(title, image, link, ..., class = "p-2 mb-3 border rounded w-100") {
  div(
    class = "col-sm-6 col-md-4",
    div(
      class = class,
      if (!is.null(image)) {
        div(
          class = "mb-3 position-relative",
          style = "height: 170px;",
          tags$img(class = "position-absolute start-50 top-50 translate-middle",
                   src = image$src,
                   alt = image$alt,
                   style = "max-height: 170px; max-width: 100%;"),
        )
      },
      div(
        h3(class = "text-uppercase", title),
        p(class = "card-text", ...),
        a(href = link, title, target = "_blank", class = "btn btn-primary")
      )
    )
  )
}

posit_hex <- function(pkg) {
  list(
    src = glue("https://github.com/rstudio/hex-stickers/raw/main/PNG/{pkg}.png"),
    alt = glue("{pkg} R package hex sticker")
  )
}

placeholder_hex <- function(pkg) {
  list(
    src = "img/placeholder.png",
    alt = glue("{pkg} R package placeholder")
  )
}