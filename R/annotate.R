#' Run the annotation pipeline on a set of documents
#'
#' Runs the clean_nlp annotators over a given corpus of text
#' using either the R, Java, or Python backend. The details for
#' which annotators to run and how to run them are specified
#' by using one of: \code{\link{init_tokenizers}},
#' \code{\link{init_spaCy}}, or \code{\link{init_coreNLP}}.
#'
#' @param input          either a vector of file names to parse, or a
#'                       character vector with one document in each
#'                       element. Specify the latter with the
#'                       as_string flag.
#' @param file           character. Location to store a compressed R
#'                       object containing the results.
#'                       If NULL, the default, no such compressed
#'                       object will be stored.
#' @param output_dir     path to the directory where the raw output
#'                       should be stored. Will be created if it does not
#'                       exist. Files currently in this location will
#'                       be overwritten. If NULL, the default, it uses a
#'                       temporary directory.
#'                       Not to be confused with \code{file}, this
#'                       location stores the raw csv
#'                       files rather than a compressed dataset.
#' @param load           logical. Once parsed, should the data be read into
#'                       R as an annotation object?
#' @param keep           logical. Once parsed, should the files be kept
#'                       on disk in \code{output_dir}?
#' @param as_strings     logical. Is the data given to \code{input} the
#'                       actual document text rather
#'                       than file names?
#' @param doc_id_offset  integer. The first document id to use. Defaults
#'                       to 0.
#' @param backend        which backend to use. Will default to the last
#'                       model to be initalized.
#' @param meta           an optional data frame to bind to the document
#'                       table
#'
#' @return  if \code{load} is true, an object of class \code{annotation}.
#'          Otherwise, a character vector giving the output location of
#'          the files.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @references
#'
#'   Manning, Christopher D., Mihai Surdeanu, John Bauer, Jenny Finkel,
#'   Steven J. Bethard, and David McClosky. 2014.
#'   \href{http://nlp.stanford.edu/pubs/StanfordCoreNlp2014.pdf}{The
#'         Stanford CoreNLP Natural Language Processing Toolkit}.
#'   In: \emph{Proceedings of the 52nd Annual Meeting of the Association
#'             for Computational Linguistics: System Demonstrations,
#'             pp. 55-60.}
#'
#' @examples
#'\dontrun{
#'annotation <- run_annotators("path/to/corpus/directory")
#'}
#'
#' @export
run_annotators <- function(input, file = NULL, output_dir = NULL, load = TRUE,
                     keep = TRUE, as_strings = FALSE, doc_id_offset = 0L,
                     backend = NULL, meta = NULL) {


  if (is.null(backend)) {
    if (volatiles$model_init_last == "")
      stop("No initialized backends found.")
    backend <- volatiles$model_init_last
  } else {
    backend <- match.arg(backend, c("tokenizers", "spaCy", "coreNLP"))
  }

  if (!is.null(meta)) {
    if (!inherits(meta, "data.frame"))
      stop("meta must be a data frame")
    if (nrow(meta) != length(input))
      stop("meta must have exactly one row for each element in input")
  }

  if (backend == "tokenizers" & !volatiles$tokenizers$init)
    stop("The tokenizers backend has not been initialized;",
         "you must run 'init_tokenizers()'.")
  if (backend == "spaCy" & !volatiles$spaCy$init)
    stop("The spaCy backend has not been initialized; you",
         "must run 'init_spaCy()'.")
  if (backend == "coreNLP" & !volatiles$coreNLP$init)
    stop("The coreNLP backend has not been initialized;",
         "you must run 'init_coreNLP()'.")

  if (is.null(output_dir))
    output_dir <- tempfile() # yes, we want tempfile and not tempdir; the
                             # latter points to a static directory that is
                             # persistent through the R session; tempfile()
                             # gives a random path *within* that directory;
                             # we are free to treat it as a directory rather
                             # than a file.

  if (!dir.exists(output_dir))
    dir.create(output_dir, FALSE, TRUE)

  if (!is.null(file) && !dir.exists(dirname(file)))
    stop("base of the file argument does not point to a known directory")

  output_dir <- file.path(Sys.glob(output_dir), "/")
  output_dir <- gsub("\\", "/", output_dir, fixed = TRUE)

  if (as_strings) {
    new_input <- NULL
    for (i in seq_along(input)) {
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
    # this is just a safe guard; in theory cannot get here
    # w/o reticuate
    if (!requireNamespace("reticulate")) {
      stop("The reticulate package is required to use the spaCy backend.")
    }

    output_loc <- system.file("py", package="cleanNLP")
    volatiles$spaCy$SpacyObj$setOutputPath(output_dir)
    volatiles$spaCy$SpacyObj$setIdOffset(as.integer(doc_id_offset + 1L))
    if (length(input) <= 1)
      input <- list(input)
    volatiles$spaCy$SpacyObj$processFiles(input)

  } else if (backend == "coreNLP") {
    # this is just a safe guard; in theory cannot get here
    # w/o rJava
    if (!requireNamespace("rJava")) {
      stop("The rJava package is required to use the coreNLP backend.")
    }

    rJava::.jcall(volatiles$coreNLP$AnnotationProcessor, "V",
                  "setOutputPath", output_dir)
    rJava::.jcall(volatiles$coreNLP$AnnotationProcessor, "V",
                  "setLanguage", volatiles$coreNLP$language)
    rJava::.jcall(volatiles$coreNLP$AnnotationProcessor, "V",
                  "setIdOffset", as.integer(doc_id_offset + 1))

    rJava::.jcall(volatiles$coreNLP$AnnotationProcessor, "V",
                  "processFiles",
                  rJava::.jarray(input), volatiles$coreNLP$coreNLP)

  } else if (backend == "tokenizers") {
    if (!requireNamespace("tokenizers")) {
      stop("The tokenizers package is required to use the",
           "tokenizers backend.")
    }

    .annotate_with_r(input, output_dir, doc_id_offset + 1)

  } else {

    stop("Invalid backend specification")

  }

  # read in the output, if desired
  load_at_all <- load | !is.null(file) | !is.null(meta)
  out <- if (load_at_all) {
    read_annotation(output_dir)
  } else {
    output_dir
  }

  # Add metadata
  if (!is.null(meta)) {
    out$document <- dplyr::bind_cols(out$document, meta)
    if (keep)
      write_annotation(out, output_dir)
  }

  # save compressed file, if desired:
  if (!is.null(file)) {
    readr::write_rds(out, file)
    if (!load) out <- file
  }

  # remove the output, if desired
  if (!keep) {
    for (this in c("coreference", "dependency", "document", "entity",
                   "sentence", "token")) {
      if (file.exists(fp <- file.path(output_dir,
                        sprintf("%s.csv", this)))) file.remove(fp)
    }
  }

  return(out)
}
