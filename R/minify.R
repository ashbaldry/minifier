#' Minify Code
#'
#' @description
#' This...
#'
#' @param code Snippet of code to minify
#' @param language Language of the code to minify. Options are \code{javascript}, \code{css}, \code{html}
#'
#' @seealso
#' \url{https://www.toptal.com/developers/javascript-minifier/api}
#' \url{https://www.toptal.com/developers/cssminifier/api}
#' \url{https://www.toptal.com/developers/html-minifier/api}
#'
#' @export
minify <- function(code, language = MINIFY_LANGUAGES) {
  language <- match.arg(language)

  if (language == "css") {
    url_separator <- ""
  } else {
    url_separator <- "-"
  }

  if (language == "js") {
    language <- "javascript"
  }

  response <- httr::POST(
    paste0("https://www.toptal.com/developers/", language, url_separator, "minifier/raw"),
    body = list(input = code),
    encode = "form"
  )

  httr::stop_for_status(response)
  httr::content(response)
}
