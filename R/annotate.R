#' Run the annotation pipeline on a set of documents
#'
#' Runs the clean_nlp annotators over a given corpus of text
#' using either the R, Java, or Python backend. The details for
#' which annotators to run and how to run them are specified
#' by using one of: \code{\link{set_spacy_properties}} or
#' \code{\link{set_java_properties}}.
#'
#' @param input          either a vector of file names to parse, or a character vector
#'                       with one document in each element. Specify the latter with the
#'                       as_string flag.
#' @param file           character. Location to store a compressed R object containing the results.
#'                       If NULL, the default, no such compressed object will be stored.
#' @param output_dir     path to the directory where the raw output should be stored. Will be
#'                       created if it does not exist. Files currently in this location will
#'                       be overwritten. If NULL, the default, it uses a temporary directory.
#'                       Not to be confused with \code{file}, this location stores the raw csv
#'                       files rather than a compressed dataset.
#' @param load           logical. Once parsed, should the data be read into R as an annotation object?
#' @param keep           logical. Once parsed, should the files be kept on disk in \code{output_dir}?
#' @param as_strings     logical. Is the data given to \code{input} the actual document text rather
#'                       than file names?
#' @param doc_id_offset  integer. The first document id to use. Defaults to 0.
#' @param backend        which backend to use. Either "tokenizers", "spaCy", or "coreNLP". If
#'                       not specified it will first see if the spaCy model is initialized and
#'                       use it if so. Otherwise it checks to see if the coreNLP one is initialized
#'                       and then uses it. If neither are found the "tokenizers" package is used instead.
#'
#' @return if \code{load} is true, an object of class \code{annotation}. Otherwise, a character
#'   vector giving the output location of the files.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @references
#'
#'   Manning, Christopher D., Mihai Surdeanu, John Bauer, Jenny Finkel, Steven J. Bethard, and
#'   David McClosky. 2014. \href{http://nlp.stanford.edu/pubs/StanfordCoreNlp2014.pdf}{The Stanford CoreNLP Natural Language Processing Toolkit}.
#'   In: \emph{Proceedings of the 52nd Annual Meeting of the Association for Computational Linguistics: System Demonstrations, pp. 55-60.}
#'
#' @examples
#'\dontrun{
#'annotation <- annotate("path/to/corpus/directory")
#'}
#'
#' @export
annotate <- function(input, file = NULL, output_dir = NULL, load = TRUE, keep = TRUE,
                      as_strings = FALSE, doc_id_offset = 0L,
                      backend = c("tokenizers", "spaCy", "coreNLP")) {

  backend <- match.arg(backend, several.ok = TRUE)

  # need to figure out which one to use; if not specified
  # and spaCy is initalized is that
  if (length(backend) > 1L) {
    if (!is.null(volatiles$spacy)) {
      backend <- "spaCy"
    } else if (!is.null(volatiles$cNLP)) {
      backend <- "coreNLP"
    } else {
      backend <- "tokenizers"
    }
  }

  if (backend == "spaCy" && is.null(volatiles$spacy))
    stop("The spaCy backend has not been initialized.")
  if (backend == "coreNLP" && is.null(volatiles$cNLP))
    stop("The coreNLP backend has not been initialized.")

  if (is.null(output_dir))
    output_dir <- tempdir()

  if (!dir.exists(output_dir))
    dir.create(output_dir, FALSE, TRUE)

  if (!is.null(file) && !dir.exists(dirname(file)))
    stop("base of the file argument does not point to a known directory")

  output_dir <- file.path(Sys.glob(output_dir), "")

  if (as_strings) {
    new_input <- NULL
    for (i in 1:length(input)) {
      this_file <- tempfile()
      new_input <- c(new_input, this_file)
      writeLines(input[i], this_file)
    }
    input <- new_input
  }

  if (length(doc_id_offset <- as.integer(doc_id_offset)) > 1L)
    warning("Only using first value of doc_id_offset")

  input <- Sys.glob(input)
  if (length(input) == 0) stop("No valid files found.")

  if (backend == "spaCy") {
    message("Annotating with Python backend using spaCy")

    # this is just a safe guard; in theory cannot get here
    # w/o reticuate
    if (!requireNamespace("reticulate")) {
      stop("The reticulate package is required to use the spaCy backend.")
    }

    output_loc <- system.file("py", package="cleanNLP")
    x <- reticulate::py_run_file(file.path(output_loc, "load_spacy.py"))
    volatiles$spacy$setOutputPath(output_dir)
    volatiles$spacy$setIdOffset(as.integer(doc_id_offset))
    if (length(input) <= 1)
      input <- list(input)
    volatiles$spacy$processFiles(input)

  } else if (backend == "coreNLP") {
    message("Annotating with Java backend using coreNLP")

    # this is just a safe guard; in theory cannot get here
    # w/o rJava
    if (!requireNamespace("rJava")) {
      stop("The rJava package is required to use the coreNLP backend.")
    }

    rJava::.jcall(volatiles$ap, "V", "setOutputPath", output_dir)
    rJava::.jcall(volatiles$ap, "V", "setLanguage", volatiles$language)
    rJava::.jcall(volatiles$ap, "V", "setIdOffset", doc_id_offset)

    rJava::.jcall(volatiles$ap, "V", "processFiles", rJava::.jarray(input), volatiles$cNLP)
  } else if (backend == "tokenizers") {
    message("Annotating with R backend using tokenizers")

    if (!requireNamespace("tokenizers")) {
      stop("The tokenizers package is required to use the tokenizers backend.")
    }

    .annotate_with_r(input, output_dir, doc_id_offset)

  } else {

    stop("Invalid backend specification")

  }

  # read in the output, if desired
  load_at_all <- load | !is.null(file)
  include_vectors <- (backend == "spaCy") & volatiles$spacy_props$vector_flag
  out <- if (load_at_all) {
    read_annotation(output_dir, include_vectors = include_vectors)
  } else {
    output_dir
  }

  # save compressed file, if desired:
  if (!is.null(file)) {
    readr::write_rds(out, file)
    if (!load) out <- file
  }

  # remove the output, if desired
  if (!keep) {
    for (this in c("coreference", "dependency", "document", "entity", "sentiment", "token", "triple")) {
      if (file.exists(fp <- file.path(output_dir, sprintf("%s.csv", this)))) file.remove(fp)
    }
  }

  return(out)
}

#' Read annotation files from disk
#'
#' Loads an annotation that has been stored as a set of csv files in a local directory.
#' This is typically created by a call to \code{\link{annotate}} or \code{\link{write_annotation}}.
#'
#' @param input_dir         path to the directory where the files are stored
#' @param include_vectors   boolean. Should word vectors be loaded as well.
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
read_annotation <- function(input_dir, include_vectors = FALSE) {

  if (file.exists(file.path(input_dir, "document.csv"))) {

    anno <- structure(list(
        coreference = readr::read_csv(file.path(input_dir, "coreference.csv"), col_types = "iiiccccciiii"),
        dependency  = readr::read_csv(file.path(input_dir, "dependency.csv"),  col_types = "iiiiicc"),
        document    = readr::read_csv(file.path(input_dir, "document.csv"),    col_types = "iTccc"),
        entity      = readr::read_csv(file.path(input_dir, "entity.csv"),      col_types = "iiiiccc"),
        sentiment   = readr::read_csv(file.path(input_dir, "sentiment.csv"),   col_types = "iiiddddd"),
        token       = readr::read_csv(file.path(input_dir, "token.csv"),       col_types = "iiiccccccii"),
        triple      = readr::read_csv(file.path(input_dir, "triple.csv"),      col_types = "icccdiiiiiiiiiii"),
        vector      = NULL
      ), class = "annotation")

    if (include_vectors) {
      vec_file <- file.path(input_dir, "vector.csv")
      if (file.exists(vec_file)) {
        vector <- scan(file.path(input_dir, "vector.csv"), sep = ",")
        anno$vector <- matrix(vector, ncol = 303L, byrow = TRUE)
      } else {
        warning("No vector file found. Ignoring request to include it.")
      }
    }


  } else {

    stop(sprintf("Cannot find the file \"%s.csv\"", file.path(input_dir, "document")))

  }

  anno
}

#' Write annotation files to disk
#'
#' Takes an annotation object and stores it as a set of files in a local directory.
#' These are stored as plain-text csv files with column headers. To save as a compressed
#' format, instead directly call the function \code{\link{saveRDS}}.
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

  readr::write_csv(annotation$coreference, file.path(output_dir, "coreference.csv"))
  readr::write_csv(annotation$dependency, file.path(output_dir, "dependency.csv"))
  readr::write_csv(annotation$document, file.path(output_dir, "document.csv"))
  readr::write_csv(annotation$entity, file.path(output_dir, "entity.csv"))
  readr::write_csv(annotation$sentiment, file.path(output_dir, "sentiment.csv"))
  readr::write_csv(annotation$token, file.path(output_dir, "token.csv"))
  readr::write_csv(annotation$triple, file.path(output_dir, "triple.csv"))
  if (!is.null(annotation$vector)) {
    utils::write.table(annotation$vector, file.path(output_dir, "vector.csv"),
                col.names = FALSE, row.names = FALSE, sep = ",")
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
#'from_CoNLLU(annotation, "/path/to/file.conllu")
#'}
#'
#' @export
from_CoNLLU <- function(file) {

  # extract data
  x <- readr::read_delim(file, delim = "\t", col_types = "iccccciccc", col_names = FALSE, na = c("_"))
  tid <- x$X1
  sid <- cumsum(tid == 1) - 1L
  id <- rep(0L, length(sid))
  word <- x$X2
  lemma <- x$X3
  upos <- x$X4
  pos <- x$X5
  relation <- x$X8
  source <- x$X7

  # create token table
  token <- dplyr::data_frame(id = id, sid = sid, tid = tid, word = word, lemma = lemma,
                upos = upos, pos = pos, speaker = NA_character_, wiki = NA_character_,
                cid = NA_integer_, cid_end = NA_integer_)

  roots <- token[tid == 1,]
  roots$tid <- 0L
  roots$word <- roots$lemma <- "ROOT"
  roots$upos <- roots$pos <- NA_character_
  token <- dplyr::bind_rows(token, roots)
  token <- token[order(token$id, token$sid, token$tid),]

  # create dependency table
  tid_target <- tid
  dep <- dplyr::data_frame(id = id, sid = sid, tid = source, sid_target = sid,
                           tid_target = tid_target, relation = relation,
                           relation_full = relation)

  # create annotation object
  anno <- list()
  anno$coreference <- structure(list(id = integer(0), rid = integer(0), mid = integer(0),
      mention = character(0), mention_type = character(0), number = character(0),
      gender = character(0), animacy = character(0), sid = integer(0),
      tid = integer(0), tid_end = integer(0), tid_head = integer(0)), .Names = c("id",
  "rid", "mid", "mention", "mention_type", "number", "gender",
  "animacy", "sid", "tid", "tid_end", "tid_head"), row.names = integer(0), class = c("tbl_df",
  "tbl", "data.frame"))

  anno$dependency <- dep

  anno$document <- structure(list(id = 0L, time = structure(NA_real_, tzone = "UTC", class = c("POSIXct",
  "POSIXt")), version = NA_character_, language = NA_character_,
      file = file), .Names = c("id", "time", "version",
  "language", "file"), row.names = c(NA, -1L), class = c("tbl_df",
  "tbl", "data.frame"))

  anno$entity <- structure(list(id = integer(0), sid = integer(0), tid = integer(0),
      tid_end = integer(0), entity_type = character(0), entity = character(0),
      entity_normalized = character(0)), .Names = c("id", "sid",
  "tid", "tid_end", "entity_type", "entity", "entity_normalized"
  ), row.names = integer(0), class = c("tbl_df", "tbl", "data.frame"
  ))

  anno$sentiment <- structure(list(id = integer(0), sid = integer(0), pred_class = integer(0),
      p0 = numeric(0), p1 = numeric(0), p2 = numeric(0), p3 = numeric(0),
      p4 = numeric(0)), .Names = c("id", "sid", "pred_class", "p0",
  "p1", "p2", "p3", "p4"), row.names = integer(0), class = c("tbl_df",
  "tbl", "data.frame"))

  anno$token <- token

  anno$triple <- structure(list(id = integer(0), subject = character(0), object = character(0),
      relation = character(0), confidence = numeric(0), be_prefix = integer(0),
      be_suffix = integer(0), of_suffix = integer(0), tmod = integer(0),
      sid = integer(0), tid_subject = integer(0), tid_subject_end = integer(0),
      tid_object = integer(0), tid_object_end = integer(0), tid_relation = integer(0),
      tid_relation_end = integer(0)), .Names = c("id", "subject",
  "object", "relation", "confidence", "be_prefix", "be_suffix",
  "of_suffix", "tmod", "sid", "tid_subject", "tid_subject_end",
  "tid_object", "tid_object_end", "tid_relation", "tid_relation_end"
  ), row.names = integer(0), class = c("tbl_df", "tbl", "data.frame"
  ))

  anno$vector <- NULL

  class(anno) <- "annotation"
  anno
}

#' Reset document ids
#'
#' Given an annotation object, this function changes all of the document ids so
#' that they are all contiguous integers, starting at the parameter \code{start_id}.
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
doc_id_reset <- function(annotation, start_id = 0L) {
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
#' a single object. All document ids are reset so that they are contiguous integers
#' starting at zero.
#'
#' @param ...   annotation objects to combine; either a single list item or all of
#'              the objects as individual inputs
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

  if (!all( sapply(obj, class) == "annotation"))
    stop("can only combine annotation objects")

  num_docs <- sapply(obj, function(anno) length(unique(anno$document$id)) )

  offset <- cumsum(num_docs)
  offset <- c(0, offset[-length(offset)])

  temp <- mapply(function(anno, os) doc_id_reset(anno, os), obj, offset, SIMPLIFY = FALSE)

  anno <- structure(list(
       coreference = dplyr::bind_rows(lapply(temp, getElement, "coreference")),
       dependency  = dplyr::bind_rows(lapply(temp, getElement, "dependency")),
       document    = dplyr::bind_rows(lapply(temp, getElement, "document")),
       entity      = dplyr::bind_rows(lapply(temp, getElement, "entity")),
       sentiment   = dplyr::bind_rows(lapply(temp, getElement, "sentiment")),
       token       = dplyr::bind_rows(lapply(temp, getElement, "token")),
       triple      = dplyr::bind_rows(lapply(temp, getElement, "triple")),
       vector      = Reduce(rbind, lapply(temp, getElement, "vector"))
  ), class = "annotation")

  anno
}

#' Extract documents from an annotation object
#'
#' Takes an annotation object and returns an annotation object containing only
#' a subset of the documents.
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
    stop("You must supply a list of ids to extract")

  id <- NULL # silence check
  new_vector <- get_vector(annotation)
  if (!is.null(new_vector))
    new_vector <- new_vector[new_vector[,1] %in% ids,]
  anno <- structure(list(
       coreference = dplyr::filter(get_coreference(annotation), id %in% ids),
       dependency  = dplyr::filter(get_dependency(annotation), id %in% ids),
       document    = dplyr::filter(get_document(annotation), id %in% ids),
       entity      = dplyr::filter(get_entity(annotation), id %in% ids),
       sentiment   = dplyr::filter(get_sentiment(annotation), id %in% ids),
       token       = dplyr::filter(get_token(annotation), id %in% ids),
       triple      = dplyr::filter(get_triple(annotation), id %in% ids),
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
  writeLines("id,sid,tid,word,lemma,upos,pos,speaker,wiki,cid,cid_end", fp)

  fp <- file.path(output_dir, "dependency.csv")
  writeLines("id,sid,tid,sid_target,tid_target,relation,relation_full", fp)

  fp <- file.path(output_dir, "entity.csv")
  writeLines("id,sid,tid,tid_end,entity_type,entity,entity_normalized", fp)

  fp <- file.path(output_dir, "document.csv")
  writeLines("id,time,version,language,uri", fp)

  fp <- file.path(output_dir, "sentiment.csv")
  writeLines("id,sid,pred_class,p0,p1,p2,p3,p4", fp)

  fp <- file.path(output_dir, "coreference.csv")
  writeLines("id,rid,mid,mention,mention_type,number,gender,animacy,sid,tid,tid_end,tid_head", fp)

  fp <- file.path(output_dir, "triple.csv")
  temp <- c("id,subject,object,relation,confidence,be_prefix,be_suffix,",
             "of_suffix,tmod,sid,tid_subject,tid_subject_end,tid_object,",
             "tid_object_end,tid_relation,tid_relation_end")
  writeLines(paste(temp, collapse = ""), fp)

  id <- doc_id_offset
  for (x in input) {
    # ADD DOC TO DOC TABLE
    df <- data_frame(id = id, time = format(Sys.time(), fmt = "%d"),
                     version = as.character(utils::packageVersion("cleanNLP")),
                     language = "n/a", uri = x)
    readr::write_csv(df, file.path(output_dir, "document.csv"), append = TRUE)

    txt <- readLines(x)
    txt <- iconv(txt, sub = "")
    txt <- paste(txt, collapse = " ")
    y <- tokenizers::tokenize_sentences(txt)[[1]]
    y <- lapply(y, tokenizers::tokenize_words, lowercase = FALSE)
    y <- lapply(y, function(v) c("ROOT", v[[1]]))

    sid <- mapply(function(u, v) rep(u, length(v)), 1:length(y), y, SIMPLIFY = FALSE)
    tid <- mapply(function(u) 0:(length(u)-1), y, SIMPLIFY = FALSE)

    df <- data_frame(id = id, sid = unlist(sid), tid = unlist(tid), word = unlist(y),
                     lemma = NA_character_, upos = NA_character_, pos = NA_character_,
                     speaker = NA_character_, wiki = NA_character_, cid = NA_integer_,
                     cid_end = NA_integer_)

    # ADD LINE TO TOKEN FILE
    readr::write_csv(df, file.path(output_dir, "token.csv"), append = TRUE)

    id <- id + 1
  }

}



