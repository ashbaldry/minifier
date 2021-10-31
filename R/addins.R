addinMinifyFile <- function() {
  filename <- rstudioapi::getSourceEditorContext()
  if (is.null(filename) || filename$path == "") {
    message("Current source has no path. Please save before continue")
    return(NULL)
  }

  language <- tools::file_ext(filename$path)

  if (!language %in% MINIFY_LANGUAGES) {
    stop("File type not currently minifiable")
  }

  minifyFile(filename$path, language)
}

addinMinifyWWWDirectory <- function() {
  working_dir <- getwd()
  if (!"www" %in% list.files(working_dir) || !R.utils::isDirectory(file.path(working_dir, "www"))) {
    message("Current working directory has no \"www\" subdirectory. Please check getwd() for current directory.")
    return(NULL)
  }

  project <- rstudioapi::getActiveProject()
  if (is.null(project)) project <- working_dir

  www_dir <- file.path(working_dir, "www")
  minifyJSDirectory(www_dir, minified_file_name = paste0(basename(project), ".min.js"))
  minifyCSSDirectory(www_dir, minified_file_name = paste0(basename(project), ".min.css"))
}
