volatiles <- new.env(parent=emptyenv())

.onLoad <- function(libname, pkgname) {
  volatiles$tokenizers <- list(init = FALSE, setup = FALSE)
  volatiles$spacy <- list(init = FALSE, setup = FALSE)
  volatiles$corenlp <- list(init = FALSE, setup = FALSE)
  volatiles$udpipe <- list(init = FALSE, setup = FALSE)
  volatiles$model_init_last <- ""
}

