.onLoad <- function(libname, pkgname) {
  volatiles$cNLP  <- NULL
  volatiles$spacy <- NULL
  volatiles$spacy_props <- list(entity_flag = TRUE, vector_flag = FALSE)
}