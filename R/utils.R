getSearchFileExtension <- function(language) {
  if (language == "javascript") {
    extension <- "js"
  } else {
    extension <- language
  }

  paste0("*.", extension, "$")
}
