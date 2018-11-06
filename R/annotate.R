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
#'                       specify what column names contain the text and
#'                       (optionally) document ids
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
#' @param doc_var        if passing a data frame, character description of the
#'                       column containing the document identifier; if this
#'                       this variable does not exist in the dataset,
#'                       automatic names will be given (or set to NULL to
#'                       force automatic names)
#' @param text_var       if passing a data frame, which column contains the
#'                       document identifier
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
                           meta = NULL,
                           doc_var = "doc_id",
                           text_var = "text") {

  # make sure there is a valid backend specified; if not
  # explicit, assume user wants the last initialized
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

    # grab the text data
    if (!(text_var %in% names(input))) {
      stop(sprintf("text_var variable '%s' not found in input", text_var))
    }

    # grab document ids
    if (!is.null(doc_var)) {
      if (doc_var %in% names(input)) {
        doc_ids <- input[[doc_var]]
      }
    }

    # grab metadata
    if (!is.null(meta)) {
      warning("both data frame and metadata provided; ignoring the latter")
    }

    if (!is.null(doc_var)) {
      non_meta_cols <- which(names(input) %in% c(doc_var, text_var))
    } else {
      non_meta_cols <- which(names(input) == text_var)
    }

    if (length(non_meta_cols) < ncol(input)) {
      meta <- input[,-non_meta_cols,drop=FALSE]
    }

    input <- input[[text_var]]
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

#' Quickly Compute Data Frame of Annotations
#'
#' Runs the clean_nlp annotators over a given corpus of text
#' and returns a data frame of annotated text with one token
#' per line. By default it will initalize the udpipe backend
#' if no annotators are found.
#'
#' @param input          either a vector of file names to parse, a
#'                       character vector with one document in each
#'                       element, or a data frame. If a data frame,
#'                       specify what column names contain the text and
#'                       (optionally) document ids
#' @param ...            additional options passed to
#'                       \code{\link{cnlp_annotate}}
#'
#' @return  a data frame of annotations with one row per token
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @examples
#'\dontrun{
#'annotation <- cnlp_quick(c("Parse this text.", "This too, as a new doc."))
#'}
#'
#' @export
cnlp_quick <- function(input, ...) {

  # if there is no valid backend specified, specify it as udpipe
  if (volatiles$model_init_last == "") {
    cnlp_init_udpipe("english")
  }

  anno <- cnlp_annotate(input, ...)
  df <- cnlp_get_token(anno, include_root = FALSE, combine = TRUE,
                       remove_na = TRUE, spaces = FALSE)
  names(df)[c(1L, 4L)] <- c("doc_id", "token")

  return(df)
}
