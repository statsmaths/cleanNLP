#' Access tokens from an annotation object
#'
#' @param annotation   an annotation object
#'
#' @export
get_token <- function(annotation) {
  annotation$token
}

#' Access dependencies from an annotation object
#'
#' @param annotation   an annotation object
#' @param get_token    logical. Should words and lemmas be attachend to the
#'                     returned dependency table.
#'
#' @importFrom   dplyr left_join select
#' @export
get_dependency <- function(annotation, get_token = FALSE) {
  dep <- annotation$dependency

  # silence R CMD check warnings
  id <- sid <- tid <- word <- lemma <- NULL

  if (get_token) {
    dep <- dplyr::left_join(dep, dplyr::select(annotation$token, id, sid, tid, word, lemma))
    dep <- dplyr::left_join(dep, dplyr::select(annotation$token, id, sid_target = sid, tid_target = tid,
                                               word_target = word, lemma_target = lemma))
  }

  dep
}

#' Access annotation metadata from an annotation object
#'
#' @param annotation   an annotation object
#'
#' @export
get_annotation <- function(annotation) {
  annotation$annotation
}

#' Access document metadata from an annotation object
#'
#' @param annotation   an annotation object
#'
#' @export
get_document <- function(annotation) {
  annotation$document
}

#' Access coreferences from an annotation object
#'
#' @param annotation   an annotation object
#'
#' @export
get_coreference <- function(annotation) {
  annotation$coreference
}

#' Access named entities from an annotation object
#'
#' @param annotation   an annotation object
#'
#' @export
get_entity <- function(annotation) {
  annotation$entity
}

#' Access sentiment scores from an annotation object
#'
#' @param annotation   an annotation object
#'
#' @export
get_sentiment <- function(annotation) {
  annotation$sentiment
}

#' Access triples from an annotation object
#'
#' @param annotation   an annotation object
#'
#' @export
get_triple <- function(annotation) {
  annotation$triple
}
