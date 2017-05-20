volatiles <- new.env(parent=emptyenv())

.onLoad <- function(libname, pkgname) {
  volatiles$tokenizers <- list(init = FALSE, setup = FALSE)
  volatiles$spaCy <- list(init = FALSE, setup = FALSE)
  volatiles$coreNLP <- list(init = FALSE, setup = FALSE)
  volatiles$model_init_last <- ""
}

