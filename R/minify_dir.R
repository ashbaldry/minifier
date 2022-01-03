#' Minify File
#'
#' @description
#' Using the
#'
#' @param directory Path to the files to be minified
#' @param single_file Should all the files be minified to a single file? Default set to \code{TRUE}
#' @param minified_dir_name,minified_file_name If \code{single_file = TRUE}, the directory and name of the minified file
#'
#' @seealso \code{\link{minify}}
#'
#' @rdname minifyDirectory
#' @export
minifyJSDirectory <- function(directory, single_file = TRUE, minified_dir_name = directory,
                              minified_file_name = paste0(basename(directory), ".min.js")) {
  if (isTRUE(single_file)) {
    minifyDirectory(directory, file.path(minified_dir_name, minified_file_name), "js")
  } else {
    minifyMultipleFiles(directory, "js")
  }
}

#' @rdname minifyDirectory
#' @export
minifyCSSDirectory <- function(directory, single_file = TRUE, minified_dir_name = directory,
                               minified_file_name = paste0(basename(directory), ".min.css")) {
  if (isTRUE(single_file)) {
    minifyDirectory(directory, file.path(minified_dir_name, minified_file_name), "css")
  } else {
    minifyMultipleFiles(directory, "css")
  }
}

#' @rdname minifyDirectory
#' @export
minifyHTMLDirectory <- function(directory, single_file = TRUE, minified_dir_name = directory,
                                minified_file_name = paste0(basename(directory), ".min.html")) {
  if (isTRUE(single_file)) {
    minifyDirectory(directory, file.path(minified_dir_name, minified_file_name), "html")
  } else {
    minifyMultipleFiles(directory, "html")
  }
}

minifyDirectory <- function(directory, minified_file_name, language = MINIFY_LANGUAGES) {
  if (!R.utils::isDirectory(directory)) {
    stop(directory, " doesn't exist")
  }

  language <- match.arg(language)

  file_names <- list.files(directory, pattern = getSearchFileExtension(language), full.names = TRUE)

  if (length(file_names) == 0) {
    message("No ", language, " files found to minify")
    return(NULL)
  }

  code <- vapply(file_names, function(file_name) paste(readLines(file_name), collapse = "\n"), character(1))
  code <- paste(code, collapse = "\n")
  minified_code <- minify(code, language)

  writeLines(minified_code, minified_file_name)
  minified_file_name
}

minifyMultipleFiles <- function(directory, language = MINIFY_LANGUAGES) {
  if (!R.utils::isDirectory(directory)) {
    stop(directory, " doesn't exist")
  }

  file_names <- list.files(directory, pattern = getSearchFileExtension(language), full.names = TRUE)

  if (length(file_names) == 0) {
    message("No files found to minify")
    return(NULL)
  } else {
    vapply(file_names, minifyFile, character(1), language = language)
  }
}
