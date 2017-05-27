#' Read annotation files from disk
#'
#' Loads an annotation that has been stored as a set of csv files in a
#' local directory. This is typically created by a call to
#' \code{\link{run_annotators}} or \code{\link{write_annotation}}.
#'
#' @param input_dir         path to the directory where the files
#'                          are stored
#'
#' @return an object of class \code{annotation}
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @examples
#'\dontrun{
#'annotation <- read_annotation("path/to/annotation")
#'}
#'
#' @export
read_annotation <- function(input_dir) {

  if (!file.exists(file.path(input_dir, "document.csv"))) {
    stop(sprintf("Cannot find the file \"%s.csv\"",  # nocov
                 file.path(input_dir, "document")))  # nocov
  }

  if (file.exists(fn <- file.path(input_dir, "coreference.csv"))) {
    coreference <- readr::read_csv(fn,
    col_types = readr::cols(id           = readr::col_integer(),
                            rid          = readr::col_integer(),
                            mid          = readr::col_integer(),
                            mention      = readr::col_character(),
                            mention_type = readr::col_character(),
                            number       = readr::col_character(),
                            gender       = readr::col_character(),
                            animacy      = readr::col_character(),
                            sid          = readr::col_integer(),
                            tid          = readr::col_integer(),
                            tid_end      = readr::col_integer(),
                            tid_head     = readr::col_integer()))
  } else {
    coreference <- structure(list(id           = integer(0),
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
  }

  if (file.exists(fn <- file.path(input_dir, "dependency.csv"))) {
    dependency <- readr::read_csv(fn,
 col_types = readr::cols(id            = readr::col_integer(),
                         sid           = readr::col_integer(),
                         tid           = readr::col_integer(),
                         tid_target    = readr::col_integer(),
                         relation      = readr::col_character(),
                         relation_full = readr::col_character()))
  } else {
    dependency <- structure(list(id            = integer(0),
                                 sid           = integer(0),
                                 tid           = integer(0),
                                 tid_target    = integer(0),
                                 relation      = character(0),
                                 relation_full = character(0)),
                            row.names = integer(0),
                            class = c("tbl_df", "tbl", "data.frame"))
  }

  if (file.exists(fn <- file.path(input_dir, "document.csv"))) {
    document <- readr::read_csv(fn,
 col_types = readr::cols(id       = readr::col_integer(),
                         time     = readr::col_datetime(),
                         version  = readr::col_character(),
                         language = readr::col_character(),
                         uri      = readr::col_character()))
  } else {
    document <- structure(list(id       = integer(0),
                               time     = structure(numeric(0),
                                  class = c("POSIXct", "POSIXt"),
                                  tzone = "UTC"),
                               version  = character(0),
                               language = character(0),
                               uri      = character(0)),
                          row.names = integer(0),
                          class = c("tbl_df", "tbl", "data.frame"))
  }

  if (file.exists(fn <- file.path(input_dir, "entity.csv"))) {
    entity <- readr::read_csv(fn,
       col_types = readr::cols(id        = readr::col_integer(),
                       sid               = readr::col_integer(),
                       tid               = readr::col_integer(),
                       tid_end           = readr::col_integer(),
                       entity_type       = readr::col_character(),
                       entity            = readr::col_character()))
  } else {
    entity <- structure(list(id                = integer(0),
                             sid               = integer(0),
                             tid               = integer(0),
                             tid_end           = integer(0),
                             entity_type       = character(0),
                             entity            = character(0)),
                        row.names = integer(0),
                        class = c("tbl_df", "tbl", "data.frame"))
  }

  if (file.exists(fn <- file.path(input_dir, "sentence.csv"))) {
    sentence <- readr::read_csv(fn,
 col_types = readr::cols(id        = readr::col_integer(),
                         sid       = readr::col_integer()))
  } else {
    sentence <- structure(list(id        = integer(0),
                               sid       = integer(0)),
                           row.names = integer(0),
                           class = c("tbl_df", "tbl", "data.frame"))
  }

  if (file.exists(fn <- file.path(input_dir, "token.csv"))) {
    token <- readr::read_csv(fn,
       col_types = readr::cols(id      = readr::col_integer(),
                        sid     = readr::col_integer(),
                        tid     = readr::col_integer(),
                        word    = readr::col_character(),
                        lemma   = readr::col_character(),
                        upos    = readr::col_character(),
                        pos     = readr::col_character(),
                        cid     = readr::col_integer()))
  } else {
    token <- structure(list(id      = integer(0),
                            sid     = integer(0),
                            tid     = integer(0),
                            word    = character(0),
                            lemma   = character(0),
                            upos    = character(0),
                            pos     = character(0),
                            cid     = integer(0)),
                        row.names   = integer(0),
                        class = c("tbl_df", "tbl", "data.frame"))
  }

  if (file.exists(fn <- file.path(input_dir, "vector.csv"))) {
    vector <- scan(file.path(input_dir, "vector.csv"), sep = ",")
    vector <- matrix(vector, ncol = 303L, byrow = TRUE)
  } else {
    vector <- matrix(numeric(0), ncol = 3L, nrow = 0L)
  }

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

#' Write annotation files to disk
#'
#' Takes an annotation object and stores it as a set of files in a local
#' directory. These are stored as plain-text csv files with column headers.
#' To save as a compressed format, instead directly call the function
#' \code{\link{saveRDS}}.
#'
#' @param annotation  annotation file being stored
#' @param output_dir  path to the directory where the files will be saved
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @examples
#'\dontrun{
#'write_annotation(annotation, "/path/to/annotation")
#'}
#'
#' @export
write_annotation <- function(annotation, output_dir) {
  if (!dir.exists(output_dir))
    dir.create(output_dir, FALSE, TRUE)

  if (nrow(annotation$coreference) > 0L)
    readr::write_csv(annotation$coreference,
        file.path(output_dir, "coreference.csv"))
  if (nrow(annotation$dependency) > 0L)
    readr::write_csv(annotation$dependency,
        file.path(output_dir, "dependency.csv"))
  if (nrow(annotation$document) > 0L)
    readr::write_csv(annotation$document,
        file.path(output_dir, "document.csv"))
  if (nrow(annotation$entity) > 0L)
    readr::write_csv(annotation$entity,
        file.path(output_dir, "entity.csv"))
  if (nrow(annotation$sentence) > 0L)
    readr::write_csv(annotation$sentence,
        file.path(output_dir, "sentence.csv"))
  if (nrow(annotation$token) > 0L)
    readr::write_csv(annotation$token,
        file.path(output_dir, "token.csv"))

  if (!is.null(annotation$vector)) {
    utils::write.table(annotation$vector,
                       file.path(output_dir, "vector.csv"),
                       col.names = FALSE,
                       row.names = FALSE,
                       sep = ",")
  }

  invisible(NULL)
}

#' Reads a CoNLL-U or CoNLL-X File
#'
#' Takes an file saved in the CoNLL-U or CoNLL-X format and converts it
#' into an annotation object. This is a lossy procedure, grabbing just
#' tokenization, lemmatization, part of speech tags, and dependencies.
#'
#' @param file  character vector giving the path to the file
#'
#' @return an annotation object with a single document
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @examples
#'\dontrun{
#'from_CoNLL(annotation, "/path/to/file.conll")
#'}
#'
#' @export
from_CoNLL <- function(file) {

  # extract data
  x <- readr::read_delim(file, delim = "\t", col_types = "iccccciccc",
                         col_names = FALSE, na = c("_"))
  tid <- x$X1
  sid <- cumsum(tid == 1)
  id <- rep(0L, length(sid))
  word <- x$X2
  lemma <- x$X3
  upos <- x$X4
  pos <- x$X5
  relation <- x$X8
  source <- x$X7

  # create token table
  token <- dplyr::data_frame(id = id + 1, sid = sid, tid = tid,
                word = word, lemma = lemma,
                upos = upos, pos = pos, speaker = NA_character_,
                wiki = NA_character_,
                cid = NA_integer_, cid_end = NA_integer_)

  roots <- token[tid == 1,]
  roots$tid <- 0L
  roots$word <- roots$lemma <- "ROOT"
  roots$upos <- roots$pos <- NA_character_
  token <- dplyr::bind_rows(token, roots)
  token <- token[order(token$id, token$sid, token$tid),]

  # create dependency table
  tid_target <- tid
  dep <- dplyr::data_frame(id = id, sid = sid, tid = source,
                            sid_target = sid,
                            tid_target = tid_target,
                            relation = relation,
                            relation_full = relation)

  # create annotation object
  anno <- list()
  anno$coreference <- structure(list( id           = integer(0),
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

  anno$dependency <- dep

  anno$document <- structure(list(id   = 0L,
                                  time = structure(NA_real_,
                                      tzone = "UTC",
                                      class = c("POSIXct", "POSIXt")),
                                  version = NA_character_,
                                  language = NA_character_,
                                  file = file), row.names = c(NA, -1L),
                              class = c("tbl_df", "tbl", "data.frame"))

  anno$entity <- structure(list( id                = integer(0),
                                 sid               = integer(0),
                                 tid               = integer(0),
                                 tid_end           = integer(0),
                                 entity_type       = character(0),
                                 entity            = character(0)),
                        row.names = integer(0),
                        class = c("tbl_df", "tbl", "data.frame"))

  anno$sentence <- structure(list(id     = integer(0),
                                  sid    = integer(0)),
                           row.names = integer(0),
                           class = c("tbl_df", "tbl", "data.frame"))

  anno$token <- token
  anno$vector <- NULL

  class(anno) <- "annotation"
  anno
}

#' Returns a CoNLL-U Document
#'
#' Given an annotation object, this function returns a CoNLL-U
#' document. The format of CoNLL-U is close to that of a deliminated
#' file, but includes blank lines to signal breaks between sentences.
#' We return a string object that can be saved to disk using the
#' function \code{readLines}. Note that CoNLL-U does not have a way
#' of distinguishing documents. Usually only one document is written
#' to a single file. If you want this behavior, see the examples.
#' Also note that this is a lossy procedure depending on the
#' annotations available, saving just tokenization, lemmatization,
#' part of speech tags, and dependencies.
#'
#' @param anno  annotation object to convert
#'
#' @return an annotation object with a single document
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @examples
#'\dontrun{
#' for (i in get_document(obama)$id) {
#'   anno <- extract_documents(obama, i)
#'   conll <- to_CoNNL(anno)
#'   writeLines(conll, sprintf("%02d.conll", i))
#' }
#'}
#'
#' @export
to_CoNNL <- function(anno) {

  tok <- get_token(anno)
  dep <- get_dependency(anno)

  # Remove "ROOT" tokens
  tok <- tok[tok$tid > 0,]

  # Need a blank line after each sentence
  new_line <- rep("", nrow(tok))
  if (max(tok$sid) > 0) {
    index <- c(which(tok$tid == 1)[-1] - 1, nrow(tok))
    new_line[index] <- "\n"
  } else new_line[length(new_line)] <- "\n"

  # Join dependencies to tokens
  if (!is.null(dep)) {
    names(dep)[3] <- "tid_source"
    names(dep)[4] <- "tid"
    dep <- dep[!duplicated(dep[,c(1,2,4)]),]
    tok <- dplyr::left_join(tok, dep)
  } else {
    tok$tid_source <- NA
    tok$relation <- NA
  }

  # Convert to characters so that we can deal with
  # NA's correctly
  tok$tid <- as.character(tok$tid)
  tok$tid_source <- as.character(tok$tid_source)

  # Replace NA's with underscore
  tok$tid[is.na(tok$tid)] <- "_"
  tok$word[is.na(tok$word)] <- "_"
  tok$lemma[is.na(tok$lemma)] <- "_"
  tok$upos[is.na(tok$upos)] <- "_"
  tok$pos[is.na(tok$pos)] <- "_"
  tok$tid_source[is.na(tok$tid_source)] <- "_"
  tok$relation[is.na(tok$relation)] <- "_"

  # Construct the output
  out <- sprintf("%s\t%s\t%s\t%s\t%s\t_\t%s\t%s\t_\t_%s",
                 tok$tid, tok$word, tok$lemma, tok$upos,
                 tok$pos, tok$tid_source, tok$relation,
                 new_line)

  return(out)
}


#' Reset document ids
#'
#' Given an annotation object, this function changes all of the document
#' ids so that they are all contiguous integers, starting at the
#' parameter \code{start_id}.
#'
#' @param annotation   annotation object to reset the IDs of
#' @param start_id     the starting document id. Defaults to 0.
#'
#' @return an annotation object with document ids updated across all tables.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @examples
#'\dontrun{
#'doc_id_reset(annotation, 10)
#'}
#'
#' @export
doc_id_reset <- function(annotation, start_id = 1L) {
  start_id <- as.integer(start_id)

  old_ids <- sort(unique(annotation$document$id))
  new_ids <- seq(start_id, start_id + length(old_ids) - 1)

  out <- lapply(annotation, function(df) {
    if (is.data.frame(df)) {
      index <- match(df$id, old_ids)
      df$id <- new_ids[index]
    } else {
      # For the vector matrix
      index <- match(df[,1], old_ids)
      df[,1] <- new_ids[index]
    }
    df
  })

  attributes(out) <- attributes(annotation)
  out
}

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
combine_documents <- function(...) {
  obj <- list(...)
  if (length(obj) == 1 && class(obj) == "list")
    obj <- obj[[1]]

  if (!all( as.character(lapply(obj, class)) == "annotation"))
    stop("can only combine annotation objects") # nocov

  num_docs <- as.character(lapply(obj,
                function(anno) length(unique(anno$document$id))))

  offset <- cumsum(num_docs)
  offset <- c(0, offset[-length(offset)]) + 1L

  temp <- mapply(function(anno, os) doc_id_reset(anno, os),
                 obj, offset, SIMPLIFY = FALSE)

  anno <- structure(list(
   coreference = dplyr::bind_rows(lapply(temp, getElement, "coreference")),
   dependency  = dplyr::bind_rows(lapply(temp, getElement, "dependency")),
   document    = dplyr::bind_rows(lapply(temp, getElement, "document")),
   entity      = dplyr::bind_rows(lapply(temp, getElement, "entity")),
   sentence    = dplyr::bind_rows(lapply(temp, getElement, "sentence")),
   token       = dplyr::bind_rows(lapply(temp, getElement, "token")),
   vector      = Reduce(rbind, lapply(temp, getElement, "vector"))
  ), class = "annotation")

  anno
}

#' Extract documents from an annotation object
#'
#' Takes an annotation object and returns an annotation object containing
#' only a subset of the documents.
#'
#' @param annotation   the object to extract from
#' @param ids          a vector of integer ids from which to extract
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
extract_documents <- function(annotation, ids) {

  if (missing(ids))
    stop("You must supply a list of ids to extract") # nocov

  id <- NULL # silence check
  new_vector <- get_vector(annotation)
  if (!is.null(new_vector))
    new_vector <- new_vector[new_vector[,1] %in% ids,]
  anno <- structure(list(
       coreference = dplyr::filter_(get_coreference(annotation), ~ id %in% ids),
       dependency  = dplyr::filter_(get_dependency(annotation), ~ id %in% ids),
       document    = dplyr::filter_(get_document(annotation), ~ id %in% ids),
       entity      = dplyr::filter_(get_entity(annotation), ~ id %in% ids),
       sentence    = dplyr::filter_(get_sentence(annotation), ~ id %in% ids),
       token       = dplyr::filter_(get_token(annotation), ~ id %in% ids),
       vector      = new_vector
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


.annotate_with_r <- function(input, output_dir, doc_id_offset) {
  dir.create(output_dir, FALSE)

  # FILE HEADERS
  fp <- file.path(output_dir, "token.csv")
  writeLines("id,sid,tid,word,lemma,upos,pos,cid", fp)

  fp <- file.path(output_dir, "document.csv")
  writeLines("id,time,version,language,uri", fp)

  id <- doc_id_offset
  for (x in input) {
    # ADD DOC TO DOC TABLE
    df <- dplyr::data_frame(id = id,
                     time = format(Sys.time(), fmt = "%dZ", tz = "UTC"),
                     version = as.character(utils::packageVersion("cleanNLP")),
                     language = "n/a", uri = x)
    readr::write_csv(df, file.path(output_dir, "document.csv"),
                      append = TRUE, na = "")

    txt <- readLines(x)
    txt <- iconv(txt, sub = "")
    txt <- paste(txt, collapse = " ")
    y <- tokenizers::tokenize_sentences(txt)[[1]]
    y <- lapply(y, tokenizers::tokenize_words, lowercase = FALSE)
    y <- lapply(y, function(v) c("ROOT", v[[1]]))

    sid <- mapply(function(u, v) rep(u, length(v)),
                  seq_along(y), y, SIMPLIFY = FALSE)
    tid <- mapply(function(u) seq_along(u) - 1L, y, SIMPLIFY = FALSE)

    df <- dplyr::data_frame(id = id, sid = unlist(sid) - 1L,
                     tid = unlist(tid), word = unlist(y),
                     lemma = NA_character_, upos = NA_character_,
                     pos = NA_character_,
                     cid = NA_integer_)

    # ADD LINE TO TOKEN FILE
    readr::write_csv(df, file.path(output_dir, "token.csv"),
                     append = TRUE, na = "")

    id <- id + 1
  }

}

