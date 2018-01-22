#' Run the annotation pipeline on a set of documents
#'
#' Runs the clean_nlp annotators over a given corpus of text
#' using either the R, Java, or Python backend. The details for
#' which annotators to run and how to run them are specified
#' by using one of: \code{\link{cnlp_init_tokenizers}},
#' \code{\link{cnlp_init_spacy}}, \code{\link{cnlp_init_udpipe}},
#' or \code{\link{cnlp_init_corenlp}}.
#'
#' @param input          either a vector of file names to parse, a
#'                       character vector with one document in each
#'                       element, or a data frame. If a data frame,
#'                       it is assumed that the first column gives the
#'                       document ids, the second the raw text, and
#'                       other columns (if present) yield metadata
#' @param as_strings     logical. Is the data given to \code{input} the
#'                       actual document text or are they file names?
#'                       If \code{NULL}, the default, will be set to
#'                       \code{FALSE} if the input points to a valid
#'                       file and \code{TRUE} otherwise.
#' @param doc_ids        optional character vector of document names
#' @param backend        which backend to use. Will default to the last
#'                       model to be initalized.
#' @param meta           an optional data frame to bind to the document
#'                       table
#'
#' @return  an object of class \code{annotation}
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @references
#'
#'   Manning, Christopher D., Mihai Surdeanu, John Bauer, Jenny Finkel,
#'   Steven J. Bethard, and David McClosky. 2014.
#'   The Stanford corenlp Natural Language Processing Toolkit.
#'   In: \emph{Proceedings of the 52nd Annual Meeting of the Association
#'             for Computational Linguistics: System Demonstrations,
#'             pp. 55-60.}
#'
#' @examples
#'\dontrun{
#'annotation <- cnlp_annotate("path/to/corpus/directory")
#'}
#'
#' @export
cnlp_annotate <- function(input,
                           as_strings = NULL,
                           doc_ids = NULL,
                           backend = NULL,
                           meta = NULL) {

  # make sure there is a valid backend specified; if not
  # explict, assume user wants the last initalized
  if (is.null(backend)) {
    if (volatiles$model_init_last == "")
      stop("No initialized backends found.")
    backend <- volatiles$model_init_last
  }

  # select the correct backend
  backend <- match.arg(tolower(backend),
                       c("tokenizers", "spacy", "corenlp", "udpipe"))

  # if the input is a data frame, check for tif
  if (is.data.frame(input)) {

    if (ncol(input) <= 1)
      stop("The input should have at least two columns if a data frame")
    if (class(input[[2]]) != "character")
      stop("The second column of the input should contain a character vector")

    names(input)[1:2] <- c("doc_id", "text")

    if (!is.null(doc_ids)) {
      warning("data frame input given along with doc_ids; ignoring the latter")
    }
    if (!is.null(meta)) {
      warning("data frame input given along with meta; ignoring the latter")
    }
    if (ncol(input) > 2L) {
      meta <- input[,seq(3,ncol(input))]
    }

    doc_ids <- input$doc_id
    input <- input$text
    as_strings <- TRUE
  }

  # if as_strings is NULL, determine whether the first strings
  # matches a valid file
  if (is.null(as_strings)) {
    if (file.exists(input[1])) {
      as_strings <- FALSE
    } else {
      as_strings <- TRUE
    }
  }

  # if metadata is present, make sure it is a data frame and
  # of the correct size
  if (!is.null(meta)) {
    if (!inherits(meta, "data.frame"))
      stop("meta must be a data frame")
    if (nrow(meta) != length(input))
      stop("meta must have exactly one row for each element in input")
  }

  # if doc ids are given, make sure same length as input
  if (!is.null(doc_ids)) {
    doc_ids <- as.character(doc_ids)
    if (length(doc_ids) != length(input))
      stop("document ids must be the same length as the input")
    if (any(duplicated(doc_ids)))
      warning("duplicated document ids given")
  }

  # now, pass parameters to the appropriate backend code
  if (backend == "tokenizers") {
    out <- annotate_with_r(input = input,
                            as_strings = as_strings
                            )
  } else if (backend == "spacy") {
    out <- annotate_with_spacy(input = input,
                                as_strings = as_strings)
  } else if (backend == "corenlp") {
    out <- annotate_with_corenlp(input = input,
                                  as_strings = as_strings)
  } else if (backend == "udpipe") {
    out <- annotate_with_udpipe(input = input,
                                 as_strings = as_strings)
  } else {
    stop("Invalid backend specification")
  }

  if (!is.null(doc_ids)) {
    out <- set_doc_ids(out, doc_ids)
  }

  return(out)
}
