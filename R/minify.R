#' Minify File
#'
#' @param file_name Path to the file to be minified
#'
#' @rdname minifyFile
#' @export
minifyJSFile <- function(file_name) {
  minifyFile(file_name, "javascript")
}

#' @rdname minifyFile
#' @export
minifyCSSFile <- function(file_name) {
  minifyFile(file_name, "css")
}

#' @rdname minifyFile
#' @export
minifyHTMLFile <- function(file_name) {
  minifyFile(file_name, "html")
}

minifyFile <- function(file_name, langauge) {
  if (!file.exists(file_name)) {
    stop(file_name, " doesn't exist")
  }

  code <- paste(readLines(file_name), collapse = "\n")
  minify(code, langauge)
}

#' Minify Code
#'
#' @param code Snippet of code to minify
#' @param language Language of the code to minify. Options are \code{javascript}, \code{css}, \code{html}
#'
#' @export
minify <- function(code, language = c("javascript", "css", "html")) {
  language <- match.arg(language)

  if (language == "css") {
    url_separator <- ""
  } else {
    url_separator <- "-"
  }

  response <- httr::POST(
    paste0("https://www.toptal.com/developers/", language, url_separator, "minifier/raw"),
    body = list(input = code),
    encode = "form"
  )

  httr::stop_for_status(response)
  httr::content(response)
}
