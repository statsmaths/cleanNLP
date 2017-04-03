volatiles = new.env(parent=emptyenv())

#' Initialize a cleanNLP backend
#'
#' This should be run prior to calling the `annotation`
#' function. Options that control the behavior of the
#' pipeline must be set using one of: \code{\link{setup_tokenizers_backend}},
#' \code{\link{setup_spaCy_backend}}, or \code{\link{setup_coreNLP_backend}}.
#' Options may be reset during a given R session; in order to
#' take effect, make a new call to this function.
#'
#' @param type           name of the backend to initialize
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @examples
#' \dontrun{
#' setup_spaCy_backend()
#' init_backend(type = "spaCy")
#' }
#' @export
init_backend <- function(type) {

  if (missing(type)) {
    stop("type must be one of: 'tokenizers', 'spaCy', 'coreNLP'.")
  }

  type <- match.arg(type, c("tokenizers", "spaCy", "coreNLP"))

  if (type == "tokenizers") {
    res <- .init_tokenizers_backend()
  }

  if (type == "spaCy") {
    res <- .init_spaCy_backend()
  }

  if (type == "coreNLP") {
    res <- .init_coreNLP_backend()
  }

  invisible(res)
}

.init_tokenizers_backend <- function() {

  if (!requireNamespace("tokenizers")) {
    stop("The tokenizers package is required to use the tokenizers backend.")
  }

  if (!volatiles$tokenizers$setup) {
    stop("The tokenizers backend has not been set up.\n",
         "Please run setup_tokenizers_backend().")
  }

  volatiles$tokenizers$init <- TRUE
  volatiles$model_init_last <- "tokenizers"

  return(NULL)
}

.init_spaCy_backend <- function() {

  if (!requireNamespace("reticulate")) {
    stop("The reticulate package is required to use the spaCy backend.")
  }

  if (!reticulate::py_module_available("spacy")) {
    stop("The spaCy module must be installed in python before using as a backend.")
  }

  if (!volatiles$spaCy$setup) {
    stop("The spaCy backend has not been set up.\n",
         "Please run setup_spaCy_backend().")
  }

  output_loc <- system.file("py", package="cleanNLP")
  volatiles$spaCy$py_file <- reticulate::py_run_file(file.path(output_loc, "load_spacy.py"))
  model_name <- volatiles$spaCy$model_name

  temp <- tryCatch({
      volatiles$spaCy$SpacyObj <- volatiles$spaCy$py_file$SpacyCleanNLP(model_name)
  }, error = function(e) {
      stop(sprintf("The model name '%s' cannot be found by spaCy.\n", model_name),
           sprintf("You can generally download models using the following\n"),
           sprintf("command in a terminal:\n\n"),
           sprintf("   python -m spacy download %s\n\n", model_name),
           sprintf("See the spaCy documentation <https://spacy.io> for more help."))
  })

  volatiles$spaCy$SpacyObj$setEntityFlag(volatiles$spaCy$entity_flag)
  volatiles$spaCy$SpacyObj$setVectorFlag(volatiles$spaCy$vector_flag)

  gc() # manually garbage collect in case we just threw
       # away a large python object; it may look small to
       # R but the spaCy pipeline is relatively large

  volatiles$spaCy$init <- TRUE
  volatiles$model_init_last <- "spaCy"

  return(NULL)
}

.init_coreNLP_backend <- function() {

  if (!requireNamespace("rJava")) {
    stop("The rJava package is required to use the coreNLP backend")
  }

  if (!volatiles$coreNLP$setup) {
    stop("The coreNLP backend has not been set up.\n",
         "Please run setup_coreNLP_backend().")
  }

  # Parse default parameters
  fp <- file.path(system.file("extdata",package="cleanNLP"), "properties.rds")
  if (!file.exists(fp)) {
    warning("running setup_coreNLP_backend_raw(\"en\") to generate properties file.")
    setup_coreNLP_backend_raw("en")
  }
  properties <- readr::read_rds(fp)
  keys <- names(properties)
  values <- as.character(properties)
  lang <- volatiles$coreNLP$language

  # Find location of the CoreNLP Libraries
  if (!file.exists(volatiles$coreNLP$lib_location)) {
    stop("Please run download_clean_nlp() in order to install required jar files.")
  } else {
    path <- Sys.glob(file.path(volatiles$coreNLP$lib_location, "*.jar"))
  }

  # Start java engine, if not already done, and add to classpath
  options(java.parameters = paste0("-Xmx", volatiles$coreNLP$mem))
  rJava::.jinit()
  rJava::.jaddClassPath(file.path(system.file("extdata", package = "cleanNLP"), "cleanNLP-0.1.jar"))
  rJava::.jaddClassPath(path)

  # Determine if the corenlp files have been loaded correctly
  jar_files <- basename(rJava::.jclassPath())
  if (!("stanford-corenlp-3.7.0.jar" %in% jar_files))
    warning("The Stanford CoreNLP (3.7.0) files were not found as expected. Proceed with caution.")
  if (lang == "en" && !("stanford-english-corenlp-2016-10-31-models.jar" %in% jar_files))
    warning("English model file has not been downloaded to lib_location. Proceed with caution.")
  if (lang == "fr" && !("stanford-french-corenlp-2016-10-31-models.jar" %in% jar_files))
    warning("French model file has not been downloaded to lib_location. Proceed with caution.")
  if (lang == "de" && !("stanford-german-corenlp-2016-10-31-models.jar" %in% jar_files))
    warning("German model file has not been downloaded to lib_location. Proceed with caution.")
  if (lang == "es" && !("stanford-spanish-corenlp-2016-10-31-models.jar" %in% jar_files))
    warning("Spanish model file has not been downloaded to lib_location. Proceed with caution.")

  # If running for the second time, reset the annotator pool
  # We create a second one anyway, but it's good to release the
  # memory explicitly.
  if (!is.null(volatiles$coreNLP$coreNLP))
    rJava::.jcall(volatiles$coreNLP$coreNLP, "V", "clearAnnotatorPool")

  # Apply properties to a java properties object
  prop <- rJava::.jnew("java.util.Properties")
  for (i in 1:length(keys))
    prop$setProperty(keys[i], values[i])

  # Load the NLP pipeline (quietly, if desired)
  if (!volatiles$coreNLP$verbose) {
    err <- rJava::.jfield("java/lang/System", , "err")
    rJava::.jcall("java/lang/System", "V", "setErr", rJava::.jnew("java/io/PrintStream",
          rJava::.jcast(rJava::.jnew("java/io/ByteArrayOutputStream"), "java/io/OutputStream")))
  } else {
    message("Loading NLP pipeline.\n(This may take several minutes. Please be patient.)")
  }

  volatiles$coreNLP$coreNLP <- rJava::.jnew("edu.stanford.nlp.pipeline.StanfordCoreNLP", prop)
  volatiles$coreNLP$AnnotationProcessor <- rJava::.jnew("edu.richmond.nlp.AnnotationProcessor")
  if (!volatiles$coreNLP$verbose) rJava::.jcall("java/lang/System", "V", "setErr", err)

  gc() # manually garbage collect in case we just threw
       # away a large Java object; it may look small to
       # R (just a pointer) but the CoreNLP pipeline is
       # very large

  volatiles$coreNLP$init <- TRUE
  volatiles$model_init_last <- "coreNLP"

  invisible(properties)
}

#' Manually set properties for the coreNLP pipeline
#'
#' This function allows for directly setting properties to be passed on to the
#' Stanford CoreNLP pipeline. This will generally be of interest to experienced
#' users who are already familiar with the general pipeline. See the function
#' \code{\link{setup_coreNLP_backend}} for a more user-friendly approach.
#'
#' @param keys     a character vector of keys giving the names of the properties to set
#' @param values   a character vector the same length of keys giving the values to set
#'                 each respective property to
#' @param clear    should the set of properties be cleared before setting these values
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'@examples
#'\dontrun{
#'setup_coreNLP_backend_raw("annotators", "segment, ssplit, tokenize, pos")
#'}
#'
#' @export
setup_coreNLP_backend_raw <- function(keys, values, clear = FALSE) {
  if (length(keys) != length(values))
    stop(sprintf("length of keys (%d) does not match length of values (%d)", length(keys), length(values)))
  if (!inherits(keys, "character"))
    stop("keys must be a character vector")
  if (!inherits(values, "character"))
    stop("values must be a character vector")

  # read current parameter file
  if (!clear) {
    fin <- file.path(system.file("extdata", package="cleanNLP"), "properties.rds")
    if (file.exists(fin))
      prop <- readr::read_rds(fin)
    else
      prop <- list()
  } else prop <- list()

  # insert new keys and values
  for (i in 1:length(keys))
    prop[[keys[i]]] <- values[i]

  # save new parameter file
  readr::write_rds(prop, file.path(system.file("extdata", package="cleanNLP"), "properties.rds"))
}

#' Easy interface for setting up the Java-based pipeline
#'
#' This function calls \code{\link{setup_coreNLP_backend_raw}}, setting all
#' of the correct properties to parse the given language and given speed. This is
#' user-friendly entry point for setting the Java properties.
#' See Details for more information about the speed codes
#'
#' @param language       a character vector describing the desired language;
#'                         should be one of: "ar", "de", "en", "es", "fr", or "zh".
#' @param speed          integer code. Sets which annotators should be loaded, based on
#'                         on how long they take to load and run. Speed 0 is the fastest,
#'                         and speed 8 is the slowest. See Details for a full description
#'                         of the levels
#' @param lib_location   a string giving the location of the CoreNLP java
#'                         files. This should point to a directory which
#'                         contains, for example the file "stanford-corenlp-*.jar",
#'                         where "*" is the version number. If missing, the function
#'                         will try to find the library in the environment variable
#'                         CORENLP_HOME, and otherwise will fail. (Java model only)
#' @param mem            a string giving the amount of memory to be assigned to the rJava
#'                         engine. For example, "6g" assigned 6 gigabytes of memory. At least
#'                         2 gigabytes are recommended at a minimum for running the CoreNLP
#'                         package. On a 32bit machine, where this is not possible, setting
#'                         "1800m" may also work. This option will only have an effect the first
#'                         time \code{init_backend} is called for the coreNLP backend, and also
#'                         will not have an effect if the java engine is already started by
#'                         another process.
#' @param verbose        boolean. Should messages from the pipeline be written to the console or
#'                         suppressed?
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @details Currently available speed codes are integers from 0 to 8. Setting speed above 2
#'          has no additional effect on the German and Spanish models. Setting above 1 has
#'          no effect on the French model. The available speed codes are:
#'\itemize{
#'  \item{"0"}{ runs just the tokenizer, sentence splitter, and part of speech tagger. Extremely fast.}
#'  \item{"1"}{ includes the dependency parsers and, for English, the sentiment tagger. Often 20-30x
#'              slower than speed 0.}
#'  \item{"2"}{ adds the named entity annotator to the parser and sentiment tagger (when available).
#'              For English models, it also includes the mentions and natlog annotators. Usually no
#'              more than twice as slow as speed 1.}
#'  \item{"3"}{ add the coreference resolution annotator to the speed 2 annotators. Depending on the corpus,
#'              this takes about 2-4x longer than the speed 2 annotators}
#'}
#'
#' We suggest starting at speed 2 and down grading to 0 if your corpus is particularly large, or upgrading
#' to 3 if you sacrifice the slowdown. If your text is not formal written text (i.e., tweets or text messages),
#' the speed 0 annotator should still work well but anything beyond that may be difficult. Semi-formal text
#' such as e-mails or transcribed speech are generally okay to run for all of the levels.
#'
#' @examples
#'\dontrun{
#'setup_coreNLP_backend("en")
#'}
#'
#' @export
setup_coreNLP_backend <- function(language, speed = 2, lib_location = NULL, mem = "12g", verbose = FALSE) {

  if (missing(language)) language <- "en"
  language_list <- c("en", "de", "fr", "es")
  language <- match.arg(arg = language, choices = language_list)
  speed <- as.integer(speed)[1]
  if (speed < 0)
    stop("speed must be set to a non-negative integer")
  if (is.null(lib_location))
    lib_location <- file.path(system.file("extdata",package="cleanNLP"), "/stanford-corenlp-full-2016-10-31")

  # set properties that are not "coreNLP" properties
  volatiles$coreNLP$language <- language
  volatiles$coreNLP$lib_location <- lib_location
  volatiles$coreNLP$mem <- mem
  volatiles$coreNLP$verbose <- verbose

  # German models
  if (language == "de" & speed == 0) {
    setup_coreNLP_backend_raw("annotators", "tokenize, ssplit, pos", clear = TRUE)
    setup_coreNLP_backend_raw("tokenize.language", "de")
    setup_coreNLP_backend_raw("pos.model", "edu/stanford/nlp/models/pos-tagger/german/german-hgc.tagger")
  }
  if (language == "de" & speed == 1) {
    setup_coreNLP_backend_raw("annotators", "tokenize, ssplit, pos, parse", clear = TRUE)
    setup_coreNLP_backend_raw("tokenize.language", "de")
    setup_coreNLP_backend_raw("pos.model", "edu/stanford/nlp/models/pos-tagger/german/german-hgc.tagger")
    setup_coreNLP_backend_raw("parse.model", "edu/stanford/nlp/models/lexparser/germanFactored.ser.gz")
  }
  if (language == "de" & speed >= 2) {
    setup_coreNLP_backend_raw("annotators", "tokenize, ssplit, pos, ner, parse", clear = TRUE)
    setup_coreNLP_backend_raw("tokenize.language", "de")
    setup_coreNLP_backend_raw("pos.model", "edu/stanford/nlp/models/pos-tagger/german/german-hgc.tagger")
    setup_coreNLP_backend_raw("ner.model", "edu/stanford/nlp/models/ner/german.hgc_175m_600.crf.ser.gz")
    setup_coreNLP_backend_raw("ner.applyNumericClassifiers", "false")
    setup_coreNLP_backend_raw("ner.useSUTime", "false")
    setup_coreNLP_backend_raw("parse.model", "edu/stanford/nlp/models/lexparser/germanFactored.ser.gz")
  }

  # English models
  if (language == "en" & speed == 0) {
    setup_coreNLP_backend_raw("annotators", "tokenize, ssplit, pos, lemma", clear = TRUE)
  }
  if (language == "en" & speed == 1) {
    setup_coreNLP_backend_raw("annotators", "tokenize, ssplit, pos, lemma, parse, depparse, sentiment", clear = TRUE)
    setup_coreNLP_backend_raw("parse.model", "edu/stanford/nlp/models/srparser/englishSR.ser.gz")
  }
  if (language == "en" & speed == 2) {
    setup_coreNLP_backend_raw("annotators",
      "tokenize, ssplit, pos, lemma, parse, depparse, sentiment, ner, mention, entitymentions, natlog", clear = TRUE)
    setup_coreNLP_backend_raw("parse.model", "edu/stanford/nlp/models/srparser/englishSR.ser.gz")
  }
  if (language == "en" & speed >= 3) {
    setup_coreNLP_backend_raw("annotators",
      "tokenize, ssplit, pos, lemma, parse, depparse, sentiment, ner, mention, entitymentions, natlog, coref", clear = TRUE)
    setup_coreNLP_backend_raw("parse.model", "edu/stanford/nlp/models/srparser/englishSR.ser.gz")
  }

  # Spanish models
  if (language == "es" & speed == 0) {
    setup_coreNLP_backend_raw("annotators", "tokenize, ssplit, pos", clear = TRUE)
    setup_coreNLP_backend_raw("tokenize.language", "es")
    setup_coreNLP_backend_raw("pos.model", "edu/stanford/nlp/models/pos-tagger/spanish/spanish-distsim.tagger")
  }
  if (language == "es" & speed == 1) {
    setup_coreNLP_backend_raw("annotators", "tokenize, ssplit, pos, parse", clear = TRUE)
    setup_coreNLP_backend_raw("tokenize.language", "es")
    setup_coreNLP_backend_raw("pos.model", "edu/stanford/nlp/models/pos-tagger/spanish/spanish-distsim.tagger")
    setup_coreNLP_backend_raw("parse.model", "edu/stanford/nlp/models/lexparser/spanishPCFG.ser.gz")
  }
  if (language == "es" & speed >= 2) {
    setup_coreNLP_backend_raw("annotators", "tokenize, ssplit, pos, ner, parse", clear = TRUE)
    setup_coreNLP_backend_raw("tokenize.language", "es")
    setup_coreNLP_backend_raw("pos.model", "edu/stanford/nlp/models/pos-tagger/spanish/spanish-distsim.tagger")
    setup_coreNLP_backend_raw("ner.model", "edu/stanford/nlp/models/ner/spanish.ancora.distsim.s512.crf.ser.gz")
    setup_coreNLP_backend_raw("ner.applyNumericClassifiers", "false")
    setup_coreNLP_backend_raw("ner.useSUTime", "false")
    setup_coreNLP_backend_raw("parse.model", "edu/stanford/nlp/models/lexparser/spanishPCFG.ser.gz")
  }

  # French models
  if (language == "fr" & speed == 0) {
    setup_coreNLP_backend_raw("annotators", "tokenize, ssplit, pos", clear = TRUE)
    setup_coreNLP_backend_raw("tokenize.language", "fr")
    setup_coreNLP_backend_raw("pos.model", "edu/stanford/nlp/models/pos-tagger/french/french.tagger")
  }
  if (language == "fr" & speed >= 1) {
    setup_coreNLP_backend_raw("annotators", "tokenize, ssplit, pos, parse", clear = TRUE)
    setup_coreNLP_backend_raw("tokenize.language", "fr")
    setup_coreNLP_backend_raw("pos.model", "edu/stanford/nlp/models/pos-tagger/french/french.tagger")
    setup_coreNLP_backend_raw("parse.model", "edu/stanford/nlp/models/lexparser/frenchFactored.ser.gz")
  }

  volatiles$coreNLP$setup <- TRUE

  invisible(NULL)
}


#' Interface for setting up the spaCy backend
#'
#' This function sets properties to parse text using the spaCy
#' python NLP pipeline. You must re-run the \code{\link{init_backend}}
#' for these changes to take place.
#'
#' @param entity_flag  boolean. Should named entities be identified.
#' @param vector_flag  boolean. Should word vectors be computed and saved.
#' @param model_name   string giving the model name for the Python/spaCy backend.
#'                       Defaults to English if NULL.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @examples
#'\dontrun{
#'setup_spaCy_backend(vector_flag = TRUE)
#'}
#'
#' @export
setup_spaCy_backend <- function(entity_flag = TRUE, vector_flag = FALSE, model_name = NULL) {
  if (is.null(model_name))
    model_name <- "en"

  volatiles$spaCy$model_name  <- model_name
  volatiles$spaCy$entity_flag <- entity_flag
  volatiles$spaCy$vector_flag <- vector_flag

  volatiles$spaCy$setup <- TRUE
}

#' Interface for setting up the tokenizers backend
#'
#' This function must be run before initializing the tokenizers
#' backend.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @examples
#'\dontrun{
#'setup_tokenizers_backend()
#'}
#'
#' @export
setup_tokenizers_backend <- function() {
  volatiles$tokenizers$setup <- TRUE
}


