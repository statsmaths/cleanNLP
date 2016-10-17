volatiles = new.env(parent=emptyenv())

#' Initialize the cleanNLP java object
#'
#' This must be run prior to calling any other cleanNLP
#' functions. Options that control the behavior of the
#' pipeline must be set using one of: \code{\link{set_properties}},
#' \code{\link{set_annotators}}, or \code{\link{set_language}}.
#' Options may be reset during a given R session; in order to take effect,
#' make a new call to this function.
#'
#' @param lib_location   a string giving the location of the CoreNLP java
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
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'@examples
#'\dontrun{
#'init_clean_nlp()
#'}
#' @importFrom  rJava .jinit .jaddClassPath .jcall .jnew .jfield .jcast
#' @export
init_clean_nlp <- function(lib_location = NULL, mem = "4g") {

  # parse input
  if (is.null(lib_location))
    lib_location <- file.path(system.file("extdata",package="cleanNLP"), "/stanford-corenlp-full-2015-12-09")

  # Parse default parameters
  fp <- file.path(system.file("extdata",package="cleanNLP"), "properties.rds")
  if (!file.exists(fp)) {
    warning("running set_language(\"en\") to generate properties file.")
    set_language("en")
  }
  properties <- readr::read_rds(fp)
  keys <- names(properties)
  values <- as.character(properties)

  # Find location of the CoreNLP Libraries
  if (!file.exists(lib_location)) {
    stop("Please run download_clean_nlp() in order to install required jar files.")
  } else {
    path <- Sys.glob(paste0(lib_location, "/*.jar"))
  }

  # Start java engine, if not already done, and add to classpath
  options(java.parameters = paste0("-Xmx", mem))
  rJava::.jinit()
  rJava::.jaddClassPath(file.path(system.file("extdata", package = "cleanNLP"), "cleanNLP-0.1.jar"))
  rJava::.jaddClassPath(path)

  # Determine if the corenlp files have been loaded correctly
  len <- length(grep("stanford-corenlp-", basename(rJava::.jclassPath())))
  if (len == 0L)
    stop("The coreNLP jar files are were not found in lib_location.")
  if (len < 4L)
    warning("The set of coreNLP jar files may be incomplete. Proceed with caution")

  if (!is.null(volatiles$cNLP))
    rJava::.jcall(volatiles$cNLP, "V", "clearAnnotatorPool")

  prop <- rJava::.jnew("java.util.Properties")
  for (i in 1:length(keys))
    .set_property_java(keys[i], values[i])

  # (quietly) load the NLP pipeline
  err <- rJava::.jfield("java/lang/System", , "err")
  rJava::.jcall("java/lang/System", "V", "setErr", .jnew("java/io/PrintStream",
          rJava::.jcast(rJava::.jnew("java/io/ByteArrayOutputStream"), "java/io/OutputStream")))
  volatiles$cNLP <- rJava::.jnew("edu.stanford.nlp.pipeline.StanfordCoreNLP", prop)
  rJava::.jcall("java/lang/System", "V", "setErr", err)

  invisible(properties)
}

.set_property_java <- function(prop, key, value) {
  prop$setProperty(key, value)
}

#' Set properties for the coreNLP pipeline
#'
#' This function allows for directly setting properties to be passed on to the
#' Stanford CoreNLP pipeline. This will generally be of interest to experienced
#' users who are already familiar with the general pipeline. See the function
#' \code{\link{set_language}} for a more user-friendly approach.
#'
#' @param keys     a character vector of keys giving the names of the properties to set
#' @param values   a character vector the same length of keys giving the values to set
#'                 each respective property to
#' @param clear    should the set of properties be cleared before setting these values
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'@examples
#'\dontrun{
#'set_properties("annotators", "segment, ssplit, tokenize, pos")
#'}
#'
#' @importFrom  readr read_rds write_rds
#' @export
set_properties <- function(keys, values, clear = FALSE) {
  if (length(keys) != length(values))
    stop(sprintf("length of keys (%d) does not match length of values (%d)", length(keys), length(values)))
  if (!inherits(keys, "character"))
    stop("keys must be a character vector")
  if (!inherits(values, "character"))
    stop("values must be a character vector")

  # read current parameter file
  if (!append) {
    prop <- readr::read_rds(file.path(system.file("extdata", package="cleanNLP"), "properties.rds"))
  } else prop <- list()

  # insert new keys and values
  for (i in 1:length(keys))
    prop[[keys[i]]] <- values[i]

  # save new parameter file
  readr::write_rds(prop, file.path(system.file("extdata", package="cleanNLP"), "properties.rds"))
}

#' Wrapper to set particular annotators
#'
#' This function calls \code{\link{set_properties}}, setting the correct set of
#' annotators that will be used in the pipeline. This function makes sure that
#' for whichever annotators are choose, all required prerequisites are also included.
#' Generally, this function will be of most use for parsing English. See the function
#' \code{\link{set_language}} for a more user-friendly approach.
#'
#' @param annotators   a character vector describing the desired annotators;
#'                     should be a subset of: "segment", "tokenize", "ssplit", "pos", "lemma",
#'                     "ner", "parse", "mention", "dcoref", "natlog", "openie",
#'                     and "sentiment"
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @examples
#'\dontrun{
#'set_annotators(c("segment", "tokenize", "ssplit", "pos", "lemma"))
#'}
#'
#' @importFrom  readr read_rds write_rds
#' @export
set_annotators <- function(annotators = NULL) {
  annotator_list <- c("segment", "tokenize", "ssplit", "pos", "lemma", "ner", "parse",
                      "depparse", "mention", "dcoref", "natlog", "openie", "sentiment")
  annotators <- match.arg(arg = annotators, choices = annotator_list, several.ok = TRUE)

  # check for consistency:
  if ("pos" %in% annotators) annotators <- c(annotators, c("tokenize", "ssplit"))
  if ("lemma" %in% annotators) annotators <- c(annotators, c("tokenize", "ssplit", "pos"))
  if ("ner" %in% annotators) annotators <- c(annotators, c("tokenize", "ssplit", "pos", "lemma"))
  if ("parse" %in% annotators) annotators <- c(annotators, c("tokenize", "ssplit"))
  if ("depparse" %in% annotators) annotators <- c(annotators, c("tokenize", "ssplit", "pos"))
  if ("dcoref" %in% annotators) annotators <- c(annotators, c("tokenize", "ssplit", "pos", "lemma", "ner", "parse"))
  if ("relation" %in% annotators) annotators <- c(annotators, c("tokenize", "ssplit", "pos", "lemma", "ner", "depparse"))
  if ("natlog" %in% annotators) annotators <- c(annotators, c("tokenize", "ssplit", "pos", "lemma", "depparse"))

  set_properties("annotators", paste0(annotators, collapse = ","))
  invisible(NULL)
}

#' Wrapper to set properties for a particular language
#'
#' This function calls \code{\link{set_properties}}, setting all of the correct
#' properties to parse the given language. This is the most user-friendly entry
#' point for setting properties. The functions \code{\link{set_annotators}} and
#' \code{\link{set_properties}}
#'
#' @param language   a character vector describing the desired language;
#'                     should be one of: "ar", "de", "en", "es", "fr", or "zh".
#' @param fast       boolean. Should options be selected to only use fast annotation
#'                     algorithms? Generally, this results in only constructing the
#'                     token and document tables.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @examples
#'\dontrun{
#'set_language("en")
#'}
#'
#' @export
set_language <- function(language, fast = FALSE) {
  language_list <- c("en", "cn", "ar", "de", "fr", "es")
  language <- match.arg(arg = language, choices = language_list)

  if (language == "ar" & fast) {
    set_properties("annotators", "segment, ssplit, pos")
    set_properties("customAnnotatorClass.segment", "edu.stanford.nlp.pipeline.ArabicSegmenterAnnotator")
    set_properties("segment.model", "edu/stanford/nlp/models/segmenter/arabic/arabic-segmenter-atb+bn+arztrain.ser.gz")
    set_properties("ssplit.boundaryTokenRegex", "[.]|[!?]+|[!\u061F]+")
    set_properties("pos.model", "edu/stanford/nlp/models/pos-tagger/arabic/arabic.tagger")
  }
  if (language == "ar" & !fast) {
    set_properties("annotators", "segment, ssplit, pos, parse")
    set_properties("customAnnotatorClass.segment", "edu.stanford.nlp.pipeline.ArabicSegmenterAnnotator")
    set_properties("segment.model", "edu/stanford/nlp/models/segmenter/arabic/arabic-segmenter-atb+bn+arztrain.ser.gz")
    set_properties("ssplit.boundaryTokenRegex", "[.]|[!?]+|[!\u061F]+")
    set_properties("pos.model", "edu/stanford/nlp/models/pos-tagger/arabic/arabic.tagger")
    set_properties("parse.model", "edu/stanford/nlp/models/lexparser/arabicFactored.ser.gz")
  }
  if (language == "de" & fast) {
    set_properties("annotators", "tokenize, ssplit, pos")
    set_properties("tokenize.language", "de")
    set_properties("pos.model", "edu/stanford/nlp/models/pos-tagger/german/german-hgc.tagger")
  }
  if (language == "de" & !fast) {
    set_properties("annotators", "tokenize, ssplit, pos, ner, parse")
    set_properties("tokenize.language", "de")
    set_properties("pos.model", "edu/stanford/nlp/models/pos-tagger/german/german-hgc.tagger")
    set_properties("ner.model", "edu/stanford/nlp/models/ner/german.hgc_175m_600.crf.ser.gz")
    set_properties("ner.applyNumericClassifiers", "false")
    set_properties("ner.useSUTime", "false")
    set_properties("parse.model", "edu/stanford/nlp/models/lexparser/germanFactored.ser.gz")
  }
  if (language == "en" & fast) {
    set_properties("annotators", "segment, tokenize, ssplit, pos, lemma")
  }
  if (language == "en" & !fast) {
    set_properties("annotators", "segment, tokenize, ssplit, pos, lemma, ner, parse, mention, dcoref, natlog, openie, sentiment")
  }
  if (language == "es" & fast) {
    set_properties("annotators", "tokenize, ssplit, pos")
    set_properties("tokenize.language", "es")
    set_properties("pos.model", "edu/stanford/nlp/models/pos-tagger/spanish/spanish-distsim.tagger")
  }
  if (language == "es" & !fast) {
    set_properties("annotators", "tokenize, ssplit, pos, ner, parse")
    set_properties("tokenize.language", "es")
    set_properties("pos.model", "edu/stanford/nlp/models/pos-tagger/spanish/spanish-distsim.tagger")
    set_properties("ner.model", "edu/stanford/nlp/models/ner/spanish.ancora.distsim.s512.crf.ser.gz")
    set_properties("ner.applyNumericClassifiers", "false")
    set_properties("ner.useSUTime", "false")
    set_properties("parse.model", "edu/stanford/nlp/models/lexparser/spanishPCFG.ser.gz")
  }
  if (language == "fr" & fast) {
    set_properties("annotators", "tokenize, ssplit, pos")
    set_properties("tokenize.language", "fr")
    set_properties("pos.model", "edu/stanford/nlp/models/pos-tagger/french/french.tagger")
  }
  if (language == "fr" & !fast) {
    set_properties("annotators", "tokenize, ssplit, pos, parse")
    set_properties("tokenize.language", "fr")
    set_properties("pos.model", "edu/stanford/nlp/models/pos-tagger/french/french.tagger")
    set_properties("parse.model", "edu/stanford/nlp/models/lexparser/frenchFactored.ser.gz")
  }
  if (language == "zh") {
    set_properties("annotators", "segment, ssplit, pos, lemma, ner, parse, mention, coref")

    # segment
    set_properties("customAnnotatorClass.segment", "edu.stanford.nlp.pipeline.ChineseSegmenterAnnotator")
    set_properties("segment.model", "edu/stanford/nlp/models/segmenter/chinese/ctb.gz")
    set_properties("segment.sighanCorporaDict", "edu/stanford/nlp/models/segmenter/chinese")
    set_properties("segment.serDictionary", "edu/stanford/nlp/models/segmenter/chinese/dict-chris6.ser.gz")
    set_properties("segment.sighanPostProcessing", "true")

    # sentence split
    set_properties("ssplit.boundaryTokenRegex", "[.]|[!?]+|[\u3002]|[\uFF01\uFF1F]+")

    # part of speech
    set_properties("pos.model", "edu/stanford/nlp/models/pos-tagger/chinese-distsim/chinese-distsim.tagger")

    # ner
    set_properties("ner.model", "edu/stanford/nlp/models/ner/chinese.misc.distsim.crf.ser.gz")
    set_properties("ner.applyNumericClassifiers", "false")
    set_properties("ner.useSUTime", "false")

    # parse
    set_properties("parse.model", "edu/stanford/nlp/models/lexparser/chineseFactored.ser.gz")

    # depparse
    set_properties("depparse.model", "edu/stanford/nlp/models/parser/nndep/PTB_CoNLL_params.txt.gz")
    set_properties("depparse.language", "chinese")

    # coref
    set_properties("coref.sieves", "ChineseHeadMatch, ExactStringMatch, PreciseConstructs, StrictHeadMatch1, StrictHeadMatch2, StrictHeadMatch3, StrictHeadMatch4, PronounMatch")
    set_properties("coref.input.type", "raw")
    set_properties("coref.postprocessing", "true")
    set_properties("coref.calculateFeatureImportance", "false")
    set_properties("coref.useConstituencyTree", "true")
    set_properties("coref.useSemantics", "false")
    set_properties("coref.md.type", "RULE")
    set_properties("coref.mode", "hybrid")
    set_properties("coref.path.word2vec", "")
    set_properties("coref.language", "zh")
    set_properties("coref.print.md.log", "false")
    set_properties("coref.defaultPronounAgreement", "true")
    set_properties("coref.zh.dict", "edu/stanford/nlp/models/dcoref/zh-attributes.txt.gz")
  }

  invisible(NULL)
}

#' Run the annotation pipeline on a set of documents
#'
#' Runs the clean_nlp annotators over a given corpus of text. The details
#' for which annotators to run and how to run them are specified by using
#' one of: \code{\link{set_properties}}, \code{\link{set_annotators}}, or
#' \code{\link{set_language}} (the latter being the most user-friendly).
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
#' @export
annotate <- function(input, output_dir = NULL, load = TRUE, keep = TRUE,
                      as_strings = FALSE, doc_id_offset = 0L) {

  if (is.null(volatiles$cNLP))
    stop("Must initilize with 'initCoreNLP'!")

  if (is.null(output_dir))
    output_dir <- tempdir()

  if (!dir.exists(output_dir))
    dir.create(dir.create(output_dir, FALSE, TRUE))

  output_dir <- file.path(Sys.glob(output_dir), "")

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
        dependency = readr::read_csv(file.path(input_dir, "dependency.csv"), col_types = "iiiiicc"),
        document = readr::read_csv(file.path(input_dir, "document.csv"), col_types = "iTccc"),
        entity = readr::read_csv(file.path(input_dir, "entity.csv"), col_types = "iiiiccc"),
        sentiment = readr::read_csv(file.path(input_dir, "sentiment.csv"), col_types = "iiiddddd"),
        token = readr::read_csv(file.path(input_dir, "token.csv"), col_types = "iiicccccii"),
        triple = readr::read_csv(file.path(input_dir, "triple.csv"), col_types = "icccdiiiiiiiiiii")
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
    df$id[index] <- new_ids
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
#' @param ...   annotation objects to combine
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
  if (!all( sapply(list(...), class) == "annotation"))
    stop("can only combine annotation objects")

  num_docs <- sapply(list(...), function(anno) length(unique(anno$document$id)) )

  offset <- cumsum(num_docs)
  offset <- c(0, offset[-length(offset)])

  temp <- mapply(function(anno, os) doc_id_reset(anno, os), list(...), offset, SIMPLIFY = FALSE)

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
