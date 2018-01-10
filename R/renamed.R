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
get_combine <- function(...) {
  message("NOTE: get_combine has been renamed cnlp_get_tif")
  cnlp_get_tif(...)
}

#' @rdname renamed
#' @export
get_coreference <- function(...) {
  message("NOTE: get_coreference has been renamed cnlp_get_coreference")
  cnlp_get_coreference(...)
}

#' @rdname renamed
#' @export
get_dependency <- function(...) {
  message("NOTE: get_dependency has been renamed cnlp_get_dependency")
  cnlp_get_dependency(...)
}

#' @rdname renamed
#' @export
get_document <- function(...) {
  message("NOTE: get_document has been renamed cnlp_get_document")
  cnlp_get_document(...)
}

#' @rdname renamed
#' @export
get_entity <- function(...) {
  message("NOTE: get_entity has been renamed cnlp_get_entity")
  cnlp_get_entity(...)
}

#' @rdname renamed
#' @export
get_sentence <- function(...) {
  message("NOTE: get_sentence has been renamed cnlp_get_sentence")
  cnlp_get_coreference(...)
}

#' @rdname renamed
#' @export
get_tfidf <- function(...) {
  message("NOTE: get_tfidf has been renamed cnlp_get_tfidf")
  res <- cnlp_get_tfidf(...)

  # support old output format for backwards compatibility
  if (attr(class(res), "package") == "Matrix") {
    mf <- match.call(expand.dots = TRUE)
    type <- mf[["type"]]
    type <- match.arg(type, c("tfidf", "tf", "idf", "vocab"))
    if (is.null(type)) type <- "tfidf"

    res <- list(temp = res, vocab = colnames(res),
                id = rownames(res))
    names(res)[1] <- type
    message("NOTE: returning legacy output format from get_tfidf")
  }

  return(res)
}

#' @rdname renamed
#' @export
get_token <- function(...) {
  message("NOTE: get_token has been renamed cnlp_get_token")
  cnlp_get_token(...)
}

#' @rdname renamed
#' @export
get_vector <- function(...) {
  message("NOTE: get_vector has been renamed cnlp_get_vector")
  cnlp_get_vector(...)
}

#' @rdname renamed
#' @export
init_coreNLP <- function(...) {
  message("NOTE: init_coreNLP has been renamed cnlp_init_corenlp")
  cnlp_init_corenlp(...)
}

#' @rdname renamed
#' @export
init_spaCy <- function(...) {
  message("NOTE: init_spaCy has been renamed cnlp_init_spacy")
  cnlp_init_spacy(...)
}

#' @rdname renamed
#' @export
init_tokenizers <- function(...) {
  message("NOTE: init_tokenizers has been renamed cnlp_init_tokenizers")
  cnlp_init_tokenizers(...)
}

#' @rdname renamed
#' @export
run_annotators <- function(...) {
  message("NOTE: run_annotators has been renamed cnlp_annotate")
  cnlp_annotate(...)
}

#' @rdname renamed
#' @export
tidy_pca <- function(...) {
  message("NOTE: tidy_pca has been renamed cnlp_pca")
  cnlp_pca(...)
}

#' @rdname renamed
#' @export
to_CoNNL <- function(...) {
  message("NOTE: to_CoNNL has been renamed cnlp_write_conll")
  cnlp_write_conll(...)
}

#' @rdname renamed
#' @export
from_CoNNL <- function(...) {
  message("NOTE: to_CoNNL has been renamed cnlp_read_conll")
  cnlp_read_conll(...)
}

#' @rdname renamed
#' @export
write_annotation <- function(...) {
  message("NOTE: write_annotation has been renamed cnlp_write_csv")
  cnlp_write_csv(...)
}

#' @rdname renamed
#' @export
read_annotation <- function(...) {
  message("NOTE: read_annotation has been renamed cnlp_read_csv")
  cnlp_read_csv(...)
}

#' @rdname renamed
#' @export
download_core_nlp <- function(...) {
  message("NOTE: download_core_nlp has been renamed cnlp_download_corenlp")
  cnlp_download_corenlp(...)
}

#' @rdname renamed
#' @export
combine_documents <- function(...) {
  message("NOTE: combine_documents has been renamed cnlp_combine_documents")
  cnlp_combine_documents(...)
}

#' @rdname renamed
#' @export
extract_documents <- function(...) {
  message("NOTE: extract_documents has been renamed cnlp_extract_documents")
  cnlp_extract_documents(...)
}

