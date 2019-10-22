#' Run the annotation pipeline on a set of documents
#'
#' Runs the clean_nlp annotators over a given corpus of text
#' using the desired backend. The details for which annotators to run and
#' how to run them are specified by using one of:
#' \code{\link{cnlp_init_stringi}}, \code{\link{cnlp_init_spacy}},
#' \code{\link{cnlp_init_udpipe}}, or \code{\link{cnlp_init_corenlp}}.
#'
#' @param input          an object containing the data to parse. Either a
#'                       character vector with the texts (optional names can
#'                       be given to provide document ids) or a data frame. The
#'                       data frame must have a column named 'text' containing
#'                       the raw text to parse; if there is a column named
#'                       'doc_id', it is treated as a a document identifier.
#'                       This conforms with corpus objects respecting the Text
#'                       Interchange Format (TIF), while allowing for some
#'                       variation.
#' @param backend        name of the backend to use. Will default to the last
#'                       model to be initalized.
#' @param verbose        logical; should annotation engine print out when it
#'                       finishes each text? Turned on by default.
#'
#' @return  an object of class \code{annotation}
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#'@examples
#'cnlp_init_stringi()
#'cnlp_annotate(un, verbose=FALSE)
#'
#' @export
cnlp_annotate <- function(input, backend = NULL, verbose = TRUE) {

  # validate input variables
  backend <- ifnull(backend, volatiles$model_init_last)
  assert(
    backend %in% c("stringi", "spacy", "corenlp", "udpipe"),
    "No initialized backends found."
  )

  if (!inherits(input, "data.frame")) {
    assert(is.vector(input), "'input' must be a data frame or vector object.")
    input <- data.frame(
      doc_id=ifnull(names(input), seq_len(length(input))),
      text=as.character(input)
    )
  }
  assert("text" %in% names(input),
    "'input' data frame must contain a column named 'text'"
  )

  # add an identifier to the dataset if not already included
  if (!("doc_id" %in% names(input)))
  {
    input$doc_id <- seq_len(nrow(input))
  }
  assert(all(!duplicated(input$doc_id)), "duplicate values found in 'id'")

  # pass to the respective backend
  if (backend == "stringi")    anno <- annotate_with_stringi(input, verbose)
  if (backend == "spacy")      anno <- annotate_with_spacy(input, verbose)
  if (backend == "corenlp")    anno <- annotate_with_corenlp(input, verbose)
  if (backend == "udpipe")     anno <- annotate_with_udpipe(input, verbose)

  return(anno)
}
