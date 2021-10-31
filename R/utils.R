MINIFY_LANGUAGES <- c("html", "js", "css")

getSearchFileExtension <- function(language) {
  paste0("*.", language, "$")
}
