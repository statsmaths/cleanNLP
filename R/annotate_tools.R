#' Combine a set of annotations
#'
#' Takes an arbitrary set of annotations and efficiently combines them into
#' a single object. All document ids are reset so that they are contiguous
#' integers starting at zero.
#'
#' @param ...   annotation objects to combine; either a single list item
#'              or all of the objects as individual inputs
#'
#' @return a single annotation object containing all of the input documents
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @examples
#'\dontrun{
#'annotation <- combine_annotators(anno01, anno02, anno03)
#'}
#'
#' @export
cnlp_combine_documents <- function(...) {
  obj <- list(...)
  if (length(obj) == 1 && class(obj) == "list")
    obj <- obj[[1]]

  if (!all( as.character(lapply(obj, class)) == "annotation"))
    stop("can only combine annotation objects") # nocov

  vector_ncol <- sapply(lapply(obj, getElement, "vector"),
                         function(v) ncol(v))

  if (length(unique(vector_ncol)) != 1) {
    stop("can only combine annotation objects because the dimension ",
         "of word embeddings is not consistent.")
  }

  if (unique(vector_ncol) == 0) {
    vector <- do.call("rbind", lapply(obj, getElement, "vector"))
  } else {
    vector <- obj[[1]][["vector"]]
  }

  anno <- structure(list(
   coreference = dplyr::bind_rows(lapply(obj, getElement, "coreference")),
   dependency  = dplyr::bind_rows(lapply(obj, getElement, "dependency")),
   document    = dplyr::bind_rows(lapply(obj, getElement, "document")),
   entity      = dplyr::bind_rows(lapply(obj, getElement, "entity")),
   sentence    = dplyr::bind_rows(lapply(obj, getElement, "sentence")),
   token       = dplyr::bind_rows(lapply(obj, getElement, "token")),
   vector      = vector
  ), class = "annotation")

  dimnames(anno[["vector"]]) <- NULL

  anno
}

#' Extract documents from an annotation object
#'
#' Takes an annotation object and returns an annotation object containing
#' only a subset of the documents.
#'
#' @param anno     the object to extract from
#' @param ids      a vector of document ids from which to extract
#'
#' @return a new annotation object
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @examples
#'\dontrun{
#'annotation <- extract_documents(anno, ids = c(0, 1, 2, 3))
#'}
#'
#' @export
cnlp_extract_documents <- function(anno, ids) {

  if (missing(ids))
    stop("You must supply a list of ids to extract") # nocov

  ids <- as.character(ids)
  these <- (anno$token$id %in% ids)
  if (nrow(anno$vector) > 0) {
    vector <- anno$vector[these,,drop=FALSE]
  } else {
    vector <- anno$vector
  }

  anno <- structure(list(
       coreference = anno$coreference[anno$coreference$id %in% ids,],
       dependency  = anno$dependency[anno$dependency$id %in% ids,],
       document    = anno$document[anno$document$id %in% ids,],
       entity      = anno$entity[anno$entity$id %in% ids,],
       sentence    = anno$sentence[anno$sentence$id %in% ids,],
       token       = anno$token[these,],
       vector      = vector
  ), class = "annotation")

  anno
}

#' Print a summary of an annotation object
#'
#' @param x    an annotation object
#' @param ...  other arguments. Currently unused.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @method print annotation
#' @export
print.annotation <- function(x, ...) {
  cat("\nA CleanNLP Annotation:\n")
  cat("  num. documents:", nrow(x$document), "\n")
  cat("\n")
  invisible(x)
}




empty_anno <- function() {

    coreference <- structure(list(id           = character(0),
                                  rid          = integer(0),
                                  mid          = integer(0),
                                  mention      = character(0),
                                  mention_type = character(0),
                                  number       = character(0),
                                  gender       = character(0),
                                  animacy      = character(0),
                                  sid          = integer(0),
                                  tid          = integer(0),
                                  tid_end      = integer(0),
                                  tid_head     = integer(0)),
                              row.names    = integer(0),
                              class = c("tbl_df", "tbl", "data.frame"))

    dependency <- structure(list(id            = character(0),
                                 sid           = integer(0),
                                 tid           = integer(0),
                                 tid_target    = integer(0),
                                 relation      = character(0),
                                 relation_full = character(0)),
                            row.names = integer(0),
                            class = c("tbl_df", "tbl", "data.frame"))

    document <- structure(list(id       = character(0),
                               time     = structure(numeric(0),
                                  class = c("POSIXct", "POSIXt"),
                                  tzone = "UTC"),
                               version  = character(0),
                               language = character(0),
                               uri      = character(0)),
                          row.names = integer(0),
                          class = c("tbl_df", "tbl", "data.frame"))

    entity <- structure(list(id                = character(0),
                             sid               = integer(0),
                             tid               = integer(0),
                             tid_end           = integer(0),
                             entity_type       = character(0),
                             entity            = character(0)),
                        row.names = integer(0),
                        class = c("tbl_df", "tbl", "data.frame"))

    sentence <- structure(list(id        = character(0),
                               sid       = integer(0)),
                           row.names = integer(0),
                           class = c("tbl_df", "tbl", "data.frame"))

    token <- structure(list(id      = character(0),
                            sid     = integer(0),
                            tid     = integer(0),
                            word    = character(0),
                            lemma   = character(0),
                            upos    = character(0),
                            pos     = character(0),
                            cid     = integer(0)),
                        row.names   = integer(0),
                        class = c("tbl_df", "tbl", "data.frame"))

  vector <- matrix(numeric(0), ncol = 0L, nrow = 0L)

  anno <- structure(list( coreference = coreference,
                          dependency  = dependency,
                          document    = document,
                          entity      = entity,
                          sentence    = sentence,
                          token       = token,
                          vector      = vector),
                    class = "annotation")

  anno
}

set_doc_ids <- function(anno, ids) {
  input_id <- sprintf("doc%d", seq_along(ids))

  for (table in c("coreference", "dependency", "document",
                  "entity", "sentence", "token")) {
    if (nrow(anno[[table]]) > 0) {
      index <- match(anno[[table]][["id"]], input_id)
      anno[[table]][["id"]] <- ids[index]
    }
  }

  return(anno)
}

