#' Run the annotation pipeline on a set of documents
#'
#' Runs the clean_nlp annotators over a given corpus of text. The details
#' for which annotators to run and how to run them are specified by using
#' one of: \code{\link{set_language}} or
#' \code{\link{set_properties}} (the former being the most user-friendly).
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
#' @importFrom  rJava .jcall .jnew
#' @importFrom  readr write_rds
#' @export
annotate <- function(input, file = NULL, output_dir = NULL, load = TRUE, keep = TRUE,
                      as_strings = FALSE, doc_id_offset = 0L) {

  if (is.null(volatiles$cNLP))
    stop("Must initilize with 'init_clean_nlp'!")

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

  # annotate the files
  rJava::.jcall(volatiles$ap, "V", "setOutputPath", output_dir)
  rJava::.jcall(volatiles$ap, "V", "setLanguage", volatiles$language)
  rJava::.jcall(volatiles$ap, "V", "setIdOffset", doc_id_offset)

  rJava::.jcall(volatiles$ap, "V", "processFiles", rJava::.jarray(input), volatiles$cNLP)

  # read in the output, if desired
  load_at_all <- load | !is.null(file)
  out <- if (load_at_all) read_annotation(output_dir) else output_dir

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
#' @param input_dir  path to the directory where the files are stored
#'
#' @return an object of class \code{annotation}
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @examples
#'\dontrun{
#'annotation <- read_annotation("path/to/annotation")
#'}
#'
#' @importFrom  readr read_csv read_rds
#' @export
read_annotation <- function(input_dir) {

  if (file.exists(file.path(input_dir, "document.csv"))) {

    anno <- structure(list(
        coreference = readr::read_csv(file.path(input_dir, "coreference.csv"), col_types = "iiiccccciiii"),
        dependency  = readr::read_csv(file.path(input_dir, "dependency.csv"),  col_types = "iiiiicc"),
        document    = readr::read_csv(file.path(input_dir, "document.csv"),    col_types = "iTccc"),
        entity      = readr::read_csv(file.path(input_dir, "entity.csv"),      col_types = "iiiiccc"),
        sentiment   = readr::read_csv(file.path(input_dir, "sentiment.csv"),   col_types = "iiiddddd"),
        token       = readr::read_csv(file.path(input_dir, "token.csv"),       col_types = "iiiccccccii"),
        triple      = readr::read_csv(file.path(input_dir, "triple.csv"),      col_types = "icccdiiiiiiiiiii")
      ), class = "annotation")

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
#' @importFrom  readr write_csv
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

  invisible(NULL)
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
    index <- match(df$id, old_ids)
    df$id <- new_ids[index]
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
#' @importFrom  dplyr bind_rows
#' @export
combine_annotators <- function(...) {
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
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @method print annotation
#' @export
print.annotation <- function(x, ...) {
  cat("\nA CleanNLP Annotation:\n")
  cat("  num. documents:", nrow(x$document), "\n")
  cat("\n")
  invisible(x)
}
