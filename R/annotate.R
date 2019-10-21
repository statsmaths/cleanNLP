#' Run the annotation pipeline on a set of documents
#'
#' Runs the clean_nlp annotators over a given corpus of text
#' using the desired backend. The details for which annotators to run and
#' how to run them are specified by using one of:
#' \code{\link{cnlp_init_stringi}}, \code{\link{cnlp_init_spacy}},
#' \code{\link{cnlp_init_udpipe}}, or \code{\link{cnlp_init_corenlp}}.
#'
#' @param input          a data frame containing the data to parse. Must
#'                       contain a column called 'text' (with the raw text to
#'                       parse) or a column called 'path' (a path to the file
#'                       that should be parsed). If both are given, the
#'                       function defers to the text column.
#' @param backend        name of the backend to use. Will default to the last
#'                       model to be initalized.
#' @param verbose        logical; should annotation engine print out when it
#'                       finishes each text? Turned on by default.
#'
#' @return  an object of class \code{annotation}
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @export
cnlp_annotate <- function(input, backend = NULL, verbose = TRUE) {

  # validate input variables
  backend <- ifnull(backend, volatiles$model_init_last)
  assert(
    backend %in% c("stringi", "spacy", "corenlp", "udpipe"),
    "No initialized backends found."
  )
  assert(inherits(input, "data.frame"), "'input' must be a data frame object.")
  assert(
    "text" %in% names(input) | "path" %in% names(input),
    "'input' must contain a column named 'text' or 'path'"
  )

  # add an identifier to the dataset if not already included
  if (!("id" %in% names(input)))
  {
    input$id <- seq_len(nrow(input))
  }
  assert(all(!duplicated(input$id)), "duplicate values found in 'id'")

  # if there is no text column, construct it from data read in from files
  if (!("text" %in% names(input)))
  {
    input$text <- rep(NA_character_, nrow(input))
    for (i in seq_len(nrow(input)))
    {
      x <- readLines(input$path[i], encoding="UTF-8")
      x <- iconv(x, sub="")
      input$text[i] <- paste(x, collapse=" ")
    }
  }

  # pass to the respective backend
  if (backend == "stringi")    anno <- annotate_with_stringi(input, verbose)
  if (backend == "spacy")      anno <- annotate_with_spacy(input, verbose)
  if (backend == "corenlp")    anno <- annotate_with_corenlp(input, verbose)
  if (backend == "udpipe")     anno <- annotate_with_udpipe(input, verbose)

  return(anno)
}
