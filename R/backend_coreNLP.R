#' Interface for initializing the coreNLP backend
#'
#' This function must be run before annotating text with
#' the tokenizers backend. It sets the properties for the
#' soreNLP engine and loads the file using rJava
#' interface provided by reticulate. See Details for more
#' information about the anno_level codes.
#'
#' @param language       a character vector describing the desired language;
#'                       should be one of: "ar", "de", "en", "es", "fr",
#'                       or "zh".
#' @param anno_level     integer code. Sets which annotators should be
#'                       loaded, based on on how long they take to load
#'                       and run. anno_level 0 is the fastest,
#'                       and anno_level 8 is the slowest. See Details for a
#'                       full description of the levels
#' @param lib_location   a string giving the location of the CoreNLP java
#'                       files. This should point to a directory which
#'                       contains, for example the file
#'                       "stanford-corenlp-*.jar",
#'                       where "*" is the version number. If missing, the
#'                       function will try to find the library in the
#'                       environment variable CORENLP_HOME, and otherwise
#'                       will fail. (Java model only)
#' @param mem            a string giving the amount of memory to be assigned
#'                       to the rJava engine. For example, "6g" assigned 6
#'                       gigabytes of memory. At least 2 gigabytes are
#'                       recommended at a minimum for running the CoreNLP
#'                       package. On a 32bit machine, where this is not
#'                       possible, setting "1800m" may also work. This
#'                       option will only have an effect the first
#'                       time \code{init_backend} is called for the coreNLP
#'                       backend, and also will not have an effect if the
#'                       java engine is already started by another process.
#' @param verbose        boolean. Should messages from the pipeline be
#'                       written to the console or suppressed?
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @details Currently available anno_level codes are integers from 0 to 8.
#'          Setting anno_level above 2
#'          has no additional effect on the German and Spanish models.
#'          Setting above 1 has
#'          no effect on the French model. The available anno_level
#'          codes are:
#'\itemize{
#'  \item{"0"}{ runs just the tokenizer, sentence splitter, and part of
#'               speech tagger. Extremely fast.}
#'  \item{"1"}{ includes the dependency parsers and, for English, the
#'              sentiment tagger. Often 20-30x
#'              slower than anno_level 0.}
#'  \item{"2"}{ adds the named entity annotator to the parser and
#'              sentiment tagger (when available).
#'              For English models, it also includes the mentions and
#'              natlog annotators. Usually no
#'              more than twice as slow as anno_level 1.}
#'  \item{"3"}{ add the coreference resolution annotator to the anno_level
#'              2 annotators. Depending on the corpus,
#'              this takes about 2-4x longer than the anno_level 2
#'              annotators}
#'}
#'
#' We suggest starting at anno_level 2 and down grading to 0 if your
#' corpus is particularly large, or upgrading
#' to 3 if you sacrifice the slowdown. If your text is not formal written
#' text (i.e., tweets or text messages),
#' the anno_level 0 annotator should still work well but anything beyond
#' that may be difficult. Semi-formal text
#' such as e-mails or transcribed speech are generally okay to run for all
#' of the levels.
#'
#' @examples
#'\dontrun{
#'init_coreNLP("en")
#'}
#'
#' @export
init_coreNLP <- function(language, anno_level = 2, lib_location = NULL,
                         mem = "6g", verbose = FALSE) {

  if (missing(language)) language <- "en"
  language_list <- c("en", "de", "fr", "es")
  language <- match.arg(arg = language, choices = language_list)
  anno_level <- as.integer(anno_level)[1]
  if (anno_level < 0)
    stop("anno_level must be set to a non-negative integer")
  if (is.null(lib_location))
    lib_location <- file.path(system.file("extdata", package="cleanNLP"),
                    "/stanford-corenlp-full-2016-10-31")

  # set properties that are not "coreNLP" properties
  volatiles$coreNLP$language <- language
  volatiles$coreNLP$lib_location <- lib_location
  volatiles$coreNLP$mem <- mem
  volatiles$coreNLP$verbose <- verbose

  # German models
  if (language == "de" & anno_level == 0) {
    .setup_coreNLP_backend_raw("annotators", "tokenize, ssplit, pos, lemma",
        clear = TRUE)
    .setup_coreNLP_backend_raw("tokenize.language", "de")
    .setup_coreNLP_backend_raw("pos.model",
      "edu/stanford/nlp/models/pos-tagger/german/german-hgc.tagger")
  }
  if (language == "de" & anno_level == 1) {
    .setup_coreNLP_backend_raw("annotators",
      "tokenize, ssplit, pos, lemma, parse, depparse", clear = TRUE)
    .setup_coreNLP_backend_raw("tokenize.language", "de")
    .setup_coreNLP_backend_raw("pos.model",
      "edu/stanford/nlp/models/pos-tagger/german/german-hgc.tagger")
    .setup_coreNLP_backend_raw("parse.model",
      "edu/stanford/nlp/models/lexparser/germanFactored.ser.gz")
  }
  if (language == "de" & anno_level >= 2) {
    .setup_coreNLP_backend_raw("annotators",
      "tokenize, ssplit, pos, lemma, ner, parse, depparse", clear = TRUE)
    .setup_coreNLP_backend_raw("tokenize.language", "de")
    .setup_coreNLP_backend_raw("pos.model",
      "edu/stanford/nlp/models/pos-tagger/german/german-hgc.tagger")
    .setup_coreNLP_backend_raw("ner.model",
      "edu/stanford/nlp/models/ner/german.conll.hgc_175m_600.crf.ser.gz")
    .setup_coreNLP_backend_raw("ner.applyNumericClassifiers", "false")
    .setup_coreNLP_backend_raw("ner.useSUTime", "false")
    .setup_coreNLP_backend_raw("parse.model",
      "edu/stanford/nlp/models/lexparser/germanFactored.ser.gz")
  }

  # English models
  if (language == "en" & anno_level == 0) {
    .setup_coreNLP_backend_raw("annotators",
      "tokenize, ssplit, pos, lemma", clear = TRUE)
  }
  if (language == "en" & anno_level == 1) {
    .setup_coreNLP_backend_raw("annotators",
      "tokenize, ssplit, pos, lemma, parse, depparse, sentiment",
      clear = TRUE)
    #.setup_coreNLP_backend_raw("parse.model",
    #  "edu/stanford/nlp/models/srparser/englishSR.ser.gz")
  }
  if (language == "en" & anno_level == 2) {
    string <- paste("tokenize, ssplit, pos, lemma, parse, depparse,",
                    "sentiment, ner, mention, entitymentions, natlog",
                    collapse = "")
    .setup_coreNLP_backend_raw("annotators", string, clear = TRUE)
    #.setup_coreNLP_backend_raw("parse.model",
    #  "edu/stanford/nlp/models/srparser/englishSR.ser.gz")
  }
  if (language == "en" & anno_level >= 3) {
    string <- paste("tokenize, ssplit, pos, lemma, parse, depparse,",
                    " sentiment, ner, mention, entitymentions, natlog, coref",
                    collapse = "")
    .setup_coreNLP_backend_raw("annotators", string, clear = TRUE)
    #.setup_coreNLP_backend_raw("parse.model",
    #  "edu/stanford/nlp/models/srparser/englishSR.ser.gz")
  }

  # Spanish models
  if (language == "es" & anno_level == 0) {
    .setup_coreNLP_backend_raw("annotators", "tokenize, ssplit, pos, lemma",
      clear = TRUE)
    .setup_coreNLP_backend_raw("tokenize.language", "es")
    .setup_coreNLP_backend_raw("pos.model",
      "edu/stanford/nlp/models/pos-tagger/spanish/spanish-distsim.tagger")
  }
  if (language == "es" & anno_level == 1) {
    .setup_coreNLP_backend_raw("annotators",
      "tokenize, ssplit, pos, lemma, parse, depparse", clear = TRUE)
    .setup_coreNLP_backend_raw("tokenize.language", "es")
    .setup_coreNLP_backend_raw("pos.model",
      "edu/stanford/nlp/models/pos-tagger/spanish/spanish-distsim.tagger")
    .setup_coreNLP_backend_raw("parse.model",
      "edu/stanford/nlp/models/lexparser/spanishPCFG.ser.gz")
  }
  if (language == "es" & anno_level >= 2) {
    .setup_coreNLP_backend_raw("annotators",
      "tokenize, ssplit, pos, lemma, ner, parse, depparse", clear = TRUE)
    .setup_coreNLP_backend_raw("tokenize.language", "es")
    .setup_coreNLP_backend_raw("pos.model",
      "edu/stanford/nlp/models/pos-tagger/spanish/spanish-distsim.tagger")
    .setup_coreNLP_backend_raw("ner.model",
      "edu/stanford/nlp/models/ner/spanish.ancora.distsim.s512.crf.ser.gz")
    .setup_coreNLP_backend_raw("ner.applyNumericClassifiers", "false")
    .setup_coreNLP_backend_raw("ner.useSUTime", "false")
    .setup_coreNLP_backend_raw("parse.model",
      "edu/stanford/nlp/models/lexparser/spanishPCFG.ser.gz")
  }

  # French models
  if (language == "fr" & anno_level == 0) {
    .setup_coreNLP_backend_raw("annotators",
      "tokenize, ssplit, pos, lemma", clear = TRUE)
    .setup_coreNLP_backend_raw("tokenize.language", "fr")
    .setup_coreNLP_backend_raw("pos.model",
      "edu/stanford/nlp/models/pos-tagger/french/french.tagger")
  }
  if (language == "fr" & anno_level >= 1) {
    .setup_coreNLP_backend_raw("annotators",
      "tokenize, ssplit, pos, lemma, parse, depparse", clear = TRUE)
    .setup_coreNLP_backend_raw("tokenize.language", "fr")
    .setup_coreNLP_backend_raw("pos.model",
      "edu/stanford/nlp/models/pos-tagger/french/french.tagger")
    .setup_coreNLP_backend_raw("parse.model",
      "edu/stanford/nlp/models/lexparser/frenchFactored.ser.gz")
  }

  invisible(.init_coreNLP_backend())
}

.setup_coreNLP_backend_raw <- function(keys, values, clear = FALSE) {
  if (length(keys) != length(values))
    stop(sprintf("length of keys (%d) does not match length of values (%d)",
      length(keys), length(values)))
  if (!inherits(keys, "character"))
    stop("keys must be a character vector")
  if (!inherits(values, "character"))
    stop("values must be a character vector")

  # read current parameter file
  if (!clear) {
    fin <- file.path(system.file("extdata", package="cleanNLP"),
      "properties.rds")
    if (file.exists(fin))
      prop <- readr::read_rds(fin)
    else
      prop <- list()
  } else prop <- list()

  # insert new keys and values
  for (i in seq_along(keys))
    prop[[keys[i]]] <- values[i]

  # save new parameter file
  readr::write_rds(prop, file.path(system.file("extdata",
    package="cleanNLP"), "properties.rds"))
}

.init_coreNLP_backend <- function() {

  if (!requireNamespace("rJava")) {
    stop("The rJava package is required to use the coreNLP backend")
  }

  # Start java engine, if not already done
  options(java.parameters = paste0("-Xmx", volatiles$coreNLP$mem))
  rJava::.jinit()

  # Make sure you have a sufficent version of Java
  ver <- rJava::.jcall("java/lang/System","S","getProperty","java.version")
  if (as.numeric(substr(ver, 1, 3)) < 1.8) {
      stop(sprintf("Your Java version (%s) in not up to date.\n", ver),
           "Please download and install Java >= 1.8")
  }

  # Parse default parameters
  fp <- file.path(system.file("extdata",package="cleanNLP"),
    "properties.rds")
  if (!file.exists(fp)) {
    .setup_coreNLP_backend_raw("en")
  }
  properties <- readr::read_rds(fp)
  keys <- names(properties)
  values <- as.character(properties)
  lang <- volatiles$coreNLP$language

  # Find location of the CoreNLP Libraries
  if (!file.exists(volatiles$coreNLP$lib_location)) {
    stop("Please run download_core_nlp() in order to",
      "install required jar files.")
  } else {
    path <- Sys.glob(file.path(volatiles$coreNLP$lib_location,
      "*.jar"))
  }

  # Start java engine, if not already done, and add to classpath
  rJava::.jaddClassPath(file.path(system.file("extdata",
    package = "cleanNLP"), "cleanNLP-0.1.jar"))
  rJava::.jaddClassPath(path)

  # Determine if the corenlp files have been loaded correctly
  jar_files <- basename(rJava::.jclassPath())
  if (!("stanford-corenlp-3.7.0.jar" %in% jar_files))
    warning("The Stanford CoreNLP (3.7.0) files were not found",
            "as expected. Proceed with caution.")
  if (lang == "en" && !("stanford-english-corenlp-2016-10-31-models.jar"
    %in% jar_files))
    warning("English model file has not been downloaded to lib_location.",
            "Proceed with caution.")
  if (lang == "fr" && !("stanford-french-corenlp-2016-10-31-models.jar"
    %in% jar_files))
    warning("French model file has not been downloaded to lib_location.",
      "Proceed with caution.")
  if (lang == "de" && !("stanford-german-corenlp-2016-10-31-models.jar"
    %in% jar_files))
    warning("German model file has not been downloaded to lib_location.",
      "Proceed with caution.")
  if (lang == "es" && !("stanford-spanish-corenlp-2016-10-31-models.jar"
    %in% jar_files))
    warning("Spanish model file has not been downloaded to lib_location.",
      "Proceed with caution.")

  # If running for the second time, reset the annotator pool
  # We create a second one anyway, but it's good to release the
  # memory explicitly.
  if (!is.null(volatiles$coreNLP$coreNLP))
    rJava::.jcall(volatiles$coreNLP$coreNLP, "V", "clearAnnotatorPool")

  # Apply properties to a java properties object
  prop <- rJava::.jnew("java.util.Properties")
  for (i in seq_along(keys))
    prop$setProperty(keys[i], values[i])

  # Load the NLP pipeline (quietly, if desired)
  if (!volatiles$coreNLP$verbose) {
    err <- rJava::.jfield("java/lang/System", , "err")
    rJava::.jcall("java/lang/System", "V", "setErr",
      rJava::.jnew("java/io/PrintStream",
      rJava::.jcast(rJava::.jnew("java/io/ByteArrayOutputStream"),
        "java/io/OutputStream")))
  } else {
    message("Loading NLP pipeline.\n(This may take several minutes.",
      "Please be patient.)")
  }

  volatiles$coreNLP$coreNLP <-
    rJava::.jnew("edu.stanford.nlp.pipeline.StanfordCoreNLP", prop)
  volatiles$coreNLP$AnnotationProcessor <-
    rJava::.jnew("edu.richmond.nlp.AnnotationProcessor")
  if (!volatiles$coreNLP$verbose)
    rJava::.jcall("java/lang/System", "V", "setErr", err)

  gc() # manually garbage collect in case we just threw
       # away a large Java object; it may look small to
       # R (just a pointer) but the CoreNLP pipeline is
       # very large

  volatiles$coreNLP$init <- TRUE
  volatiles$model_init_last <- "coreNLP"

  invisible(NULL)
}
