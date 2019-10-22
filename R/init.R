#' Interface for initializing the spacy backend
#'
#' This function must be run before annotating text with
#' the spacy backend. It sets the properties for the
#' spacy engine and loads the file using the R to Python
#' interface provided by reticulate.
#'
#' @param model_name    string giving the model name for the spacy backend.
#'                      Defaults to "en" (English) if set to NULL.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @examples
#'\dontrun{
#'cnlp_init_spacy(vector_flag = TRUE)
#'}
#'
#' @export
cnlp_init_spacy <- function(model_name=NULL) {

  check_python()
  volatiles$spacy$model_name  <- ifnull(model_name, "en")

  volatiles$spacy$obj <- volatiles$cleannlp$spacy$spacyCleanNLP(
    volatiles$spacy$model_name
  )
  assert(
    !is.null(volatiles$spacy$obj$nlp),
    sprintf(
      "model %s not found; use cnlp_download_spacy(\"%s\") to install",
      volatiles$spacy$model_name,
      volatiles$spacy$model_name
    )
  )
  volatiles$spacy$init <- TRUE
  volatiles$model_init_last <- "spacy"
}

#' Interface for initializing the udpipe backend
#'
#' This function must be run before annotating text with
#' the udpipe backend. It will parse in English by default,
#' but you can load other models as well.
#'
#' @param model_name   string giving the model namel.
#'                     Defaults to "english" if NULL.
#'                     Ignored if \code{model_path} is not NULL.
#' @param model_path   provide a full path to a model file.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @examples
#'\dontrun{
#'cnlp_init_udpipe(model_name = "english")
#'}
#'
#' @export
cnlp_init_udpipe <- function(model_name = NULL, model_path = NULL)
{
  model_name <- ifnull(model_name, "english")
  model_loc <- system.file("extdata", package="cleanNLP")

  if (is.null(model_path))
  {
    model_loc <- system.file("extdata", package="cleanNLP")
    model_path <- Sys.glob(file.path(model_loc,
                                    sprintf("%s-*.udpipe", model_name)))[1]

    # If model does not exist, download it
    if (is.na(model_path)) {
      udpipe::udpipe_download_model(
        language = model_name,
        model_dir = model_loc
      )
    }

    model_path <- Sys.glob(file.path(model_loc,
                                    sprintf("%s-*.udpipe", model_name)))[1]
  }

  volatiles$udpipe$model_name   <- ifnull(model_name, "english")
  volatiles$udpipe$model_path   <- model_path
  volatiles$udpipe$model_obj    <- udpipe::udpipe_load_model(model_path)

  volatiles$udpipe$init <- TRUE
  volatiles$model_init_last <- "udpipe"

}

#' Interface for initializing the standard R backend
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
#'cnlp_init_tokenizers()
#'}
#'
#' @export
cnlp_init_stringi <- function(locale = NULL) {

  volatiles$stringi$locale   <- ifnull(locale, stringi::stri_locale_get())
  volatiles$stringi$init     <- TRUE
  volatiles$model_init_last  <- "stringi"

}

#' Interface for initializing the coreNLP backend
#'
#' This function must be run before annotating text with
#' the coreNLP backend. It sets the properties for the
#' spacy engine and loads the file using the R to Python
#' interface provided by reticulate.
#'
#' @param lang        string giving the language name for the corenlp backend.
#'                    Defaults to "en" (English) if set to NULL.
#' @param models_dir  directory where model files are located. Set to NULL to
#'                    use the default.
#'
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @examples
#'\dontrun{
#'cnlp_init_corenlp()
#'}
#'
#' @export
cnlp_init_corenlp <- function(lang=NULL, models_dir=NULL) {

  check_python()

  volatiles$corenlp$lang <- ifnull(lang, "en")
  volatiles$corenlp$models_dir <- ifnull(
    models_dir,
    volatiles$cleannlp$corenlp$default_model_dir()
  )
  assert(
    volatiles$corenlp$lang %in%
      stringi::stri_sub(dir(volatiles$corenlp$models_dir), 1, 2),
    sprintf(
      "model %s not found; use cnlp_download_corenlp(\"%s\") to install",
      volatiles$corenlp$lang,
      volatiles$corenlp$lang
    )
  )

  volatiles$corenlp$obj <- volatiles$cleannlp$corenlp$corenlpCleanNLP(
    volatiles$corenlp$lang,
    volatiles$corenlp$models_dir
  )
  volatiles$corenlp$init <- TRUE
  volatiles$model_init_last <- "corenlp"

}

check_python <- function() {

  disc <- reticulate::py_discover_config(required_module="cleannlp")
  assert(
    !is.null(disc$required_module_path),
    "Python module 'cleannlp' not found. Install with:\n  pip install cleannlp"
  )

  assert(
    reticulate::py_module_available("cleannlp"),
    paste(c(
      "The 'cleannlp' appears to be available on your system, however\n",
      "the reticulate package has selected an alternative version of Python\n",
      "to the one where you installed the module. Restart R and run:\n\n",
      "   library(cleanNLP)\n\n",
      "prior to running any other code. If that still produces this error,\n",
      "restart R and manually select the version of Python before running\n",
      "any other functions with:\n\n",
      sprintf("   use_python(\"%s\")\n\n", disc$python)
    ), collapse=" ")
  )

  if (is.null(volatiles$cleannlp))
  {
    volatiles$cleannlp <- reticulate::import("cleannlp")
  }
}
