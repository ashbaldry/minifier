#' Minify File
#'
#' @description
#' Using the
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

minifyFile <- function(file_name, language) {
  if (!file.exists(file_name)) {
    stop(file_name, " doesn't exist")
  }

  code <- paste(readLines(file_name), collapse = "\n")
  minified_code <- minify(code, language)

  minified_file_name <- sub("(\\.\\w+)$", ".min\\1", file_name)
  writeLines(minified_code, minified_file_name)
  minified_file_name
}
