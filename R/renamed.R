#' Renamed functions
#'
#' These functions have been renamed. For the most
#' part they should now just be called with the prefix
#' 'cnlp_'. See individual warning messages for the
#' particular calling structure.
#'
#' @param ... options passed to the newly named function
#' @name renamed

#' @rdname renamed
#' @export
get_coreference <- function(...) {
  stop("ERROR: get_coreference has been renamed cnlp_get_coreference")
}

#' @rdname renamed
#' @export
get_dependency <- function(...) {
  stop("ERROR: get_dependency has been renamed cnlp_get_dependency")
}

#' @rdname renamed
#' @export
get_document <- function(...) {
  stop("ERROR: get_document has been renamed cnlp_get_document")
}

#' @rdname renamed
#' @export
get_entity <- function(...) {
  stop("ERROR: get_entity has been renamed cnlp_get_entity")
}

#' @rdname renamed
#' @export
get_sentence <- function(...) {
  stop("ERROR: get_sentence has been renamed cnlp_get_sentence")
}

#' @rdname renamed
#' @export
get_tfidf <- function(...) {
  stop("ERROR: get_tfidf has been renamed cnlp_utils_tfidf")
}

#' @rdname renamed
#' @export
get_token <- function(...) {
  stop("ERROR: get_token has been renamed cnlp_get_token")
}

#' @rdname renamed
#' @export
get_vector <- function(...) {
  stop("ERROR: get_vector has been renamed cnlp_get_vector")
}

#' @rdname renamed
#' @export
init_coreNLP <- function(...) {
  message("ERROR: init_coreNLP has been renamed cnlp_init_corenlp")
}

#' @rdname renamed
#' @export
init_spaCy <- function(...) {
  message("ERROR: init_spaCy has been renamed cnlp_init_spacy")
}

#' @rdname renamed
#' @export
init_tokenizers <- function(...) {
  message("ERROR: init_tokenizers has been renamed cnlp_init_tokenizers")
}

#' @rdname renamed
#' @export
run_annotators <- function(...) {
  message("ERROR: run_annotators has been renamed cnlp_annotate")
}

#' @rdname renamed
#' @export
tidy_pca <- function(...) {
  message("ERROR: tidy_pca has been renamed cnlp_utils_pca")
}

#' @rdname renamed
#' @export
to_CoNNL <- function(...) {
  message("ERROR: to_CoNNL has been renamed cnlp_write_conll")
}

#' @rdname renamed
#' @export
from_CoNNL <- function(...) {
  message("ERROR: to_CoNNL has been renamed cnlp_read_conll")
}

#' @rdname renamed
#' @export
write_annotation <- function(...) {
  message("ERROR: write_annotation has been renamed cnlp_write_csv")
}

#' @rdname renamed
#' @export
read_annotation <- function(...) {
  message("ERROR: read_annotation has been renamed cnlp_read_csv")
}

#' @rdname renamed
#' @export
download_core_nlp <- function(...) {
  message("ERROR: download_core_nlp has been renamed cnlp_download_corenlp")
}

#' @rdname renamed
#' @export
combine_documents <- function(...) {
  message("ERROR: combine_documents has been renamed cnlp_combine_documents")
}

#' @rdname renamed
#' @export
extract_documents <- function(...) {
  message("ERROR: extract_documents has been renamed cnlp_extract_documents")
}

