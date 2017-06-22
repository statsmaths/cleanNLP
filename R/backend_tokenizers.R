#' Interface for initializing the tokenizers backend
#'
#' This function must be run before annotating text with
#' the tokenizers backend.
#'
#' @param locale   string giving the locale name to
#'                 pass to the stringi functions. If
#'                 \code{NULL}, the default locale is
#'                 selected
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @examples
#'\dontrun{
#'init_spaCy_tokenizers()
#'}
#'
#' @export
init_tokenizers <- function(locale = NULL) {
  invisible(.init_tokenizers_backend(locale))
}

.init_tokenizers_backend <- function(locale = NULL) {

  if (!requireNamespace("stringi")) {
    stop("The stringi package is required to", # nocov
         "use this backend.")                  # nocov
  }

  if (is.null(locale)) {
    locale <- stringi::stri_locale_get()
  }

  volatiles$tokenizers$init <- TRUE
  volatiles$tokenizers$locale <- locale
  volatiles$model_init_last <- "tokenizers"

  return(NULL)
}
