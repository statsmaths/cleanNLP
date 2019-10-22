#' Download model files needed for spacy
#'
#' The cleanNLP package does not supply the model files required
#' for using the spacy backend. These files can be downloaded
#' with this function. If you need more control, download directly from
#' Python.
#'
#' @param model_name   string giving the model namel.
#'                     Defaults to "en".
#'
#'@examples
#'\dontrun{
#'cnlp_download_spacy(model_name="en")
#'cnlp_download_spacy(model_name="de")
#'}
#' @export
cnlp_download_spacy <- function(model_name="en") {
  spacy <- reticulate::import("spacy")
  spacy$cli$download(model_name)
}

#' Download model files needed for coreNLP
#'
#' The cleanNLP package does not supply the model files required
#' for using the coreNLP backend. These files can be downloaded
#' with this function. If you need more control, download directly from
#' Python.
#'
#' @param lang   string giving the languange code. Defaults to "en".
#'
#'@examples
#'\dontrun{
#'cnlp_download_corenlp(lang="en")
#'cnlp_download_corenlp(lang="de")
#'}
#' @export
cnlp_download_corenlp <- function(lang="en") {
  stanfordnlp <- reticulate::import("stanfordnlp")
  stanfordnlp$download(lang, force=TRUE)
}
