#' Interface for initializing the tokenizers backend
#'
#' This function must be run before annotating text with
#' the tokenizers backend.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @examples
#'\dontrun{
#'init_spaCy_tokenizers()
#'}
#'
#' @export
init_tokenizers <- function() {
  invisible(.init_tokenizers_backend())
}

.init_tokenizers_backend <- function() {

  if (!requireNamespace("tokenizers")) {
    stop("The tokenizers package is required to use the tokenizers backend.")
  }

  volatiles$tokenizers$init <- TRUE
  volatiles$model_init_last <- "tokenizers"

  return(NULL)
}