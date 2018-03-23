#' Read annotation files from disk
#'
#' Loads an annotation that has been stored as a set of csv files in a
#' local directory. This is typically created by a call to
#' \code{\link{cnlp_annotate}} or \code{\link{cnlp_write_csv}}.
#'
#' @param input_dir         path to the directory where the files
#'                          are stored
#'
#' @return an object of class \code{annotation}
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @examples
#'\dontrun{
#'annotation <- cnlp_read_csv("path/to/annotation")
#'}
#'
#' @export
cnlp_read_csv <- function(input_dir) {

  if (!file.exists(file.path(input_dir, "document.csv"))) {
    stop(sprintf("Cannot find the file \"%s.csv\"",  # nocov
                 file.path(input_dir, "document")))  # nocov
  }

  anno <- empty_anno()

  if (file.exists(fn <- file.path(input_dir, "coreference.csv"))) {
    temp <- utils::read.csv(fn, stringsAsFactors = FALSE)
    if (nrow(temp) > 0) {
      anno$coreference <- temp
      class(anno$coreference) <- c("tbl_df", "tbl", "data.frame")
    }
  }

  if (file.exists(fn <- file.path(input_dir, "dependency.csv"))) {
    temp <- utils::read.csv(fn, stringsAsFactors = FALSE)
    if (nrow(temp) > 0) {
      anno$dependency <- temp
      class(anno$dependency) <- c("tbl_df", "tbl", "data.frame")
    }
  }

  if (file.exists(fn <- file.path(input_dir, "document.csv"))) {
    temp <- utils::read.csv(fn, stringsAsFactors = FALSE)
    if (nrow(temp) > 0) {
      anno$document <- temp
      class(anno$document) <- c("tbl_df", "tbl", "data.frame")
    }
  }

  if (file.exists(fn <- file.path(input_dir, "entity.csv"))) {
    temp <- utils::read.csv(fn, stringsAsFactors = FALSE)
    if (nrow(temp) > 0) {
      anno$entity <- temp
      class(anno$entity) <- c("tbl_df", "tbl", "data.frame")
    }
  }

  if (file.exists(fn <- file.path(input_dir, "sentence.csv"))) {
    temp <- utils::read.csv(fn, stringsAsFactors = FALSE)
    if (nrow(temp) > 0) {
      anno$sentence <- temp
      class(anno$sentence) <- c("tbl_df", "tbl", "data.frame")
    }
  }

  if (file.exists(fn <- file.path(input_dir, "token.csv"))) {
    temp <- utils::read.csv(fn, stringsAsFactors = FALSE)
    if (nrow(temp) > 0) {
      anno$token <- temp
      class(anno$token) <- c("tbl_df", "tbl", "data.frame")
    }
  }

  if (file.exists(fn <- file.path(input_dir, "vector.csv"))) {
    ncol <- ncol(utils::read.table(file.path(input_dir, "vector.csv"),
                              nrows = 1,
                              sep = ",",
                              header = FALSE))
    vector <- scan(file.path(input_dir, "vector.csv"), sep = ",",
                   quiet = TRUE)
    anno$vector <- matrix(vector, ncol = ncol, byrow = TRUE)
  }

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
cnlp_write_csv <- function(annotation, output_dir) {
  if (!dir.exists(output_dir))
    dir.create(output_dir, FALSE, TRUE)

  if (nrow(annotation$coreference) > 0L)
    utils::write.csv(annotation$coreference,
        file.path(output_dir, "coreference.csv"),
        row.names = FALSE)
  if (nrow(annotation$dependency) > 0L)
    utils::write.csv(annotation$dependency,
        file.path(output_dir, "dependency.csv"),
        row.names = FALSE)
  if (nrow(annotation$document) > 0L)
    utils::write.csv(annotation$document,
        file.path(output_dir, "document.csv"),
        row.names = FALSE)
  if (nrow(annotation$entity) > 0L)
    utils::write.csv(annotation$entity,
        file.path(output_dir, "entity.csv"),
        row.names = FALSE)
  if (nrow(annotation$sentence) > 0L)
    utils::write.csv(annotation$sentence,
        file.path(output_dir, "sentence.csv"),
        row.names = FALSE)
  if (nrow(annotation$token) > 0L)
    utils::write.csv(annotation$token,
        file.path(output_dir, "token.csv"),
        row.names = FALSE)

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
#'cnlp_read_conll(annotation, "/path/to/file.conll")
#'}
#'
#' @export
cnlp_read_conll <- function(file) {

  # extract data
  x <- utils::read.delim(file, delim = "\t",
                   col_names = FALSE, na.strings = c("_"),
                   comment.char = "#")
  tid <- as.numeric(x$X1)
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
                upos = upos, pos = pos,
                cid = NA_integer_)

  roots <- token[tid == 1,]
  roots$tid <- 0L
  roots$word <- roots$lemma <- "ROOT"
  roots$upos <- roots$pos <- NA_character_
  token <- dplyr::bind_rows(token, roots)
  token <- token[order(token$id, token$sid, token$tid),]

  # create dependency table
  tid_target <- tid
  dep <- dplyr::data_frame(id = id + 1, sid = sid, tid = source,
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
#' for (i in cnlp_get_document(obama)$id) {
#'   anno <- extract_documents(obama, i)
#'   conll <- cnlp_write_conll(anno)
#'   writeLines(conll, sprintf("%02d.conll", i))
#' }
#'}
#'
#' @export
cnlp_write_conll <- function(anno) {

  tok <- cnlp_get_token(anno)
  dep <- cnlp_get_dependency(anno)

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


