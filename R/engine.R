volatiles = new.env(parent=emptyenv())

#' Initialize the cleanNLP java object
#'
#' This must be run prior to calling any other cleanNLP
#' functions. Options that control the behavior of the
#' pipeline must be set using \code{\link{set_cleanNLP}}.
#' You may reset the options and call this function multiple
#' times in order to specify a different set of parameter.
#'
#' @importFrom  rJava .jinit .jaddClassPath .jcall .jnew .jfield .jcast
#' @export
init_clean_nlp <- function() {

  # Parse default parameters
  fp <- file.path(system.file("extdata",package="coreNLP"), "preferences.rds")
  if (file.exists(fp)) {
    preferences <- readr::read_rds(fp)
  } else {
    warning("Using default parameters. Run setCleanNLP() to override.")
    preferences <- list(type = "english_fast",
      libLoc = paste0(system.file("extdata",package="cleanNLP"), "/stanford-corenlp-full-2015-12-09"),
      mem = "4g")
  }
  type <- preferences$type
  libLoc <- preferences$libLoc
  mem <- preferences$mem

  # Find location of the CoreNLP Libraries
  if (!file.exists(libLoc)) {
    stop("Please run downloadCleanNLP() in order to install required jar files.")
  } else {
    path <- Sys.glob(paste0(libLoc,"/*.jar"))
  }

  # Start java engine, if not already done, and add to classpath
  options(java.parameters = paste0("-Xmx", mem))
  rJava::.jinit()
  rJava::.jaddClassPath(file.path(system.file("extdata",package="cleanNLP"), "cleanNLP-0.1.jar"))
  rJava::.jaddClassPath(path)

  # Determine if the corenlp files have been loaded correctly
  len <- length(grep("stanford-corenlp-", basename(rJava::.jclassPath())))
  if (len == 0L)
    stop("The coreNLP jar files are were not found in libLoc.")
  if (len < 4L)
    warning("The set of coreNLP jar files may be incomplete. Proceed with caution")

  if (!is.null(volatiles$cNLP))
    rJava::.jcall(volatiles$cNLP, "V", "clearAnnotatorPool")

  prop <- rJava::.jnew("java.util.Properties")
  prop <- set_properties(prop, type)

  # (quietly) load the NLP pipeline
  err <- rJava::.jfield("java/lang/System", , "err")
  rJava::.jcall("java/lang/System", "V", "setErr", .jnew("java/io/PrintStream",
          rJava::.jcast(rJava::.jnew("java/io/ByteArrayOutputStream"),"java/io/OutputStream")))
  volatiles$cNLP <- rJava::.jnew("edu.stanford.nlp.pipeline.StanfordCoreNLP", prop)
  rJava::.jcall("java/lang/System", "V", "setErr", err)

  invisible(preferences)
}

set_properties <- function(prop, type) {
  if (type == "english") {
    prop$setProperty("annotators", "tokenize, ssplit, pos, lemma, ner, parse, mention, dcoref, natlog, openie, sentiment")
  }
  if (type == "english_fast") {
    prop$setProperty("annotators", "tokenize, ssplit, pos, lemma, ner, parse")
  }
  if (type == "english_very_fast") {
    prop$setProperty("annotators", "tokenize, ssplit, pos, lemma")
  }

  prop
}

#' Store properties used when initalizing the CoreNLP pipeline
#'
#' @param type           type of model to load. See details for options.
#' @param libLoc         a string giving the location of the CoreNLP java
#'                       files. This should point to a directory which
#'                       contains, for example the file "stanford-corenlp-*.jar",
#'                       where "*" is the version number. If missing, the function
#'                       will try to find the library in the environment variable
#'                       CORENLP_HOME, and otherwise will fail.
#' @param mem            a string giving the amount of memory to be assigned to the rJava
#'                       engine. For example, "6g" assigned 6 gigabytes of memory. At least
#'                       2 gigabytes are recommended at a minimum for running the CoreNLP
#'                       package. On a 32bit machine, where this is not possible, setting
#'                       "1800m" may also work. This option will only have an effect the first
#'                       time \code{initCoreNLP} is called, and also will not have an effect if
#'                       the java engine is already started by a seperate process.
#' @importFrom  readr write_rds
#' @export
set_cleanNLP <- function(type = c("english", "english_fast", "english_very_fast", "arabic", "chinese", "french", "german", "spanish"),
      libLoc = NULL, mem = "4g") {

  type <- match.arg(type)
  preferences <- list(type = type, libLoc = libLoc, mem = mem)
  readr::write_rds(preferences, file.path(system.file("extdata",package="coreNLP"), "preferences.rds"))

}

#' Run the annotation pipeline on a set of documents
#'
#' @param input          either a vector of file names to parse, or a character vector
#'                       with one document in each element. Specify the latter with the
#'                       as_string flag.
#' @param output_dir     path to the directory where the output should be stored. Will be
#'                       created if it does not exist. Files currently in this location will
#'                       be overwritten. If NULL, the default, it uses a temporary directory.
#' @param load           logical. Once parsed, should the data be read into R as an annotation object
#' @param keep           logical. Once parsed, should the files be kept on disk.
#' @param as_strings     logical. Is the data given to \code{input} the actual document text rather
#'                       file names.
#' @param doc_id_offset  integer. The first document id to use. Defaults to 0.
#'
#' @importFrom  rJava .jcall .jnew
#' @export
annotate <- function(input, output_dir = NULL, load = TRUE, keep = TRUE,
                      as_strings = FALSE, doc_id_offset = 0L) {

  if (is.null(volatiles$cNLP))
    stop("Must initilize with 'initCoreNLP'!")

  if (is.null(output_dir))
    output_dir <- tempdir()

  if (!dir.exists(output_dir <- file.path(Sys.glob(output_dir), "")))
    dir.create(dir.create("~/Desktop", FALSE, TRUE))

  if (length(doc_id_offset <- as.integer(doc_id_offset)) > 1L)
    warning("Only using first value of doc_id_offset")

  input <- Sys.glob(input)

  # annotate the files
  lang <- "en"
  ap <- rJava::.jnew("edu.richmond.nlp.AnnotationProcessor", output_dir, lang, doc_id_offset);
  rJava::.jcall(ap, "V", "processFiles", rJava::.jarray(input), volatiles$cNLP)

  # read in the output, if desired
  out <- if (load) read_annotation(output_dir) else output_dir

  # remove the output, if desired
  if (!keep) {
    for (this in c("annotation", "coreference", "dependency", "document", "entity",
      "sentiment", "token", "triple")) {
      if (file.exists(fp <- file.path(output_dir, sprintf("%s.csv", this)))) file.remove(fp)
    }
  }

  return(out)
}

#' Read annotation files from disk
#'
#' @param input_dir  path to the directory where the files are stored
#'
#' @importFrom  readr read_csv
#' @export
read_annotation <- function(input_dir) {

  anno <- structure(list(
      annotation = readr::read_csv(file.path(input_dir, "annotation.csv"), col_types = "ic"),
      coreference = readr::read_csv(file.path(input_dir, "coreference.csv"), col_types = "iiiccccciiii"),
      dependency = readr::read_csv(file.path(input_dir, "dependency.csv"), col_types = "iiiiicc"),
      document = readr::read_csv(file.path(input_dir, "document.csv"), col_types = "iTccc"),
      entity = readr::read_csv(file.path(input_dir, "entity.csv"), col_types = "iiiicc"),
      sentiment = readr::read_csv(file.path(input_dir, "sentiment.csv"), col_types = "iiiddddd"),
      token = readr::read_csv(file.path(input_dir, "token.csv"), col_types = "iiicccccii"),
      triple = readr::read_csv(file.path(input_dir, "triple.csv"), col_types = "icccdiiiiiiiiiii")
      ), class = "annotation")

  attributes(anno)$num_docs <- nrow(anno$document)
  attributes(anno)$annotators <- sort(unique(anno$annotation$annotator))

  anno
}

#' Write annotation files to disk
#'
#' @param annotation  annotation file being stored
#' @param output_dir  path to the directory where the files will be saved
#'
#' @importFrom  readr write_csv
#' @export
save_annotation <- function(annotation, output_dir) {
  if (!dir.exists(output_dir))
    dir.create(output_dir, FALSE, TRUE)

  readr::write_csv(annotation$annotation, file.path(output_dir, "annotation.csv"))
  readr::write_csv(annotation$coreference, file.path(output_dir, "coreference.csv"))
  readr::write_csv(annotation$dependency, file.path(output_dir, "dependency.csv"))
  readr::write_csv(annotation$document, file.path(output_dir, "document.csv"))
  readr::write_csv(annotation$entity, file.path(output_dir, "entity.csv"))
  readr::write_csv(annotation$sentiment, file.path(output_dir, "sentiment.csv"))
  readr::write_csv(annotation$token, file.path(output_dir, "token.csv"))
  readr::write_csv(annotation$triple, file.path(output_dir, "triple.csv"))

}

#' Reset document ids
#'
#' @param annotation   annotation object to reset the IDs of
#' @param start_id     the starting document id. Defaults to 0.
#'
#' @export
doc_id_reset <- function(annotation, start_id = 0L) {
  start_id <- as.integer(start_id)

  old_ids <- sort(unique(annotation$document$id))
  new_ids <- seq(start_id, start_id + length(old_ids) - 1)

  out <- lapply(annotation, function(df) {
    index <- match(df$id, old_ids)
    df$id[index] <- new_ids
    df
  })

  attributes(out) <- attributes(annotation)
  out
}

#' Combine a set of annotations
#'
#' @param ...   annotation objects to combine
#'
#' @importFrom  dplyr bind_rows
#' @export
combine_annotators <- function(...) {
  if (!all( sapply(list(...), class) == "annotation"))
    stop("can only combine annotation objects")

  num_docs <- sapply(list(...), function(anno) length(unique(anno$document$id)) )

  offset <- cumsum(num_docs)
  offset <- c(0, offset[-length(offset)])

  temp <- mapply(function(anno, os) doc_id_reset(anno, os), list(...), offset, SIMPLIFY = FALSE)

  anno <- structure(list(
       annotation = dplyr::bind_rows(lapply(temp, getElement, "annotation")),
       coreference = dplyr::bind_rows(lapply(temp, getElement, "coreference")),
       dependency = dplyr::bind_rows(lapply(temp, getElement, "dependency")),
       document = dplyr::bind_rows(lapply(temp, getElement, "document")),
       entity = dplyr::bind_rows(lapply(temp, getElement, "entity")),
       sentiment = dplyr::bind_rows(lapply(temp, getElement, "sentiment")),
       token = dplyr::bind_rows(lapply(temp, getElement, "token")),
       triple = dplyr::bind_rows(lapply(temp, getElement, "triple"))
  ), class = "annotation")

  anno
}

#' Print a summary of an annotation object
#'
#' @param x    an annotation object
#' @param ...  other arguments. Currently unused.
#'
#' @method print annotation
#' @export
print.annotation = function(x, ...) {
  cat("\nA CleanNLP Annotation:\n")
  cat("  num. documents:", nrow(x$document), "\n")
  cat("\n")
  invisible(x)
}