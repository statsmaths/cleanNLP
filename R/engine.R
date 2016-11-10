volatiles = new.env(parent=emptyenv())

#' Initialize the cleanNLP java object
#'
#' This must be run prior to calling any other cleanNLP
#' functions. Options that control the behavior of the
#' pipeline must be set using one of: \code{\link{set_language}} or
#' \code{\link{set_properties}}. Options may be reset during a given R session;
#' in order to take effect, make a new call to this function.
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
#'                       the java engine is already started by a separate process.
#' @param verbose        boolean. Should messages from the pipeline be written to the console or
#'                       suppressed?
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'@examples
#'\dontrun{
#'init_clean_nlp()
#'}
#' @importFrom  rJava .jinit .jaddClassPath .jcall .jnew .jfield .jcast
#' @export
init_clean_nlp <- function(lib_location = NULL, mem = "12g", verbose = TRUE) {

  # parse input
  if (is.null(lib_location))
    lib_location <- file.path(system.file("extdata",package="cleanNLP"), "/stanford-corenlp-full-2016-10-31")

  # Parse default parameters
  fp <- file.path(system.file("extdata",package="cleanNLP"), "properties.rds")
  if (!file.exists(fp)) {
    warning("running set_language(\"en\") to generate properties file.")
    set_language("en")
  }
  properties <- readr::read_rds(fp)
  keys <- names(properties)
  values <- as.character(properties)
  if (is.null(volatiles$language)) volatiles$language <- ""
  lang <- volatiles$language

  # Find location of the CoreNLP Libraries
  if (!file.exists(lib_location)) {
    stop("Please run download_clean_nlp() in order to install required jar files.")
  } else {
    path <- Sys.glob(file.path(lib_location, "*.jar"))
  }

  # Start java engine, if not already done, and add to classpath
  options(java.parameters = paste0("-Xmx", mem))
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
  if (!is.null(volatiles$cNLP))
    rJava::.jcall(volatiles$cNLP, "V", "clearAnnotatorPool")

  # Apply properties to a java properties object
  prop <- rJava::.jnew("java.util.Properties")
  for (i in 1:length(keys))
    prop$setProperty(keys[i], values[i])

  # Load the NLP pipeline (quietly, if desired)
  message("Loading NLP pipeline.\n(This may take several minutes. Please be patient.)")
  if (!verbose) {
    err <- rJava::.jfield("java/lang/System", , "err")
    rJava::.jcall("java/lang/System", "V", "setErr", .jnew("java/io/PrintStream",
          rJava::.jcast(rJava::.jnew("java/io/ByteArrayOutputStream"), "java/io/OutputStream")))
  }
  volatiles$cNLP <- rJava::.jnew("edu.stanford.nlp.pipeline.StanfordCoreNLP", prop)
  volatiles$ap <- rJava::.jnew("edu.richmond.nlp.AnnotationProcessor")
  if (!verbose) rJava::.jcall("java/lang/System", "V", "setErr", err)
  message("NLP pipeline finished loading.")

  invisible(properties)
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

#' Easy interface for setting up the pipeline
#'
#' This function calls \code{\link{set_properties}}, setting all of the correct
#' properties to parse the given language and given speed. This is the most
#' user-friendly entry point for setting properties. The function
#' \code{\link{set_properties}} provides more fine grained control.
#' See Details for more information about the speed codes
#'
#' @param language   a character vector describing the desired language;
#'                     should be one of: "ar", "de", "en", "es", "fr", or "zh".
#' @param speed      integer code. Sets which annotators should be loaded, based on
#'                     on how long they take to load and run. Speed 0 is the fastest,
#'                     and speed 8 is the slowest. See Details for a full description
#'                     of the levels
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
#'  \item{"3"}{ adds the WikiDict annotator on top of the speed 2 annotators. This take a while to
#'              load (often upwards of 5 minutes), but once loaded is only about 50\% slower than speed 2.}
#'  \item{"4"}{ adds the OpenIE triples annotator on top of the speed 2 annotators. Generally takes about
#'              twice as long as speed 2.}
#'  \item{"5"}{ add the coreference resolution annotator to the speed 2 annotators. Depending on the corpus,
#'              this takes about 2-4x longer than the speed 2 annotators}
#'  \item{"6"}{ adds both the WikiDict and coreference resolution annotator to the speed 2 annotators. The
#'              speed difference compared to speed 5 is marginal, other than the increased start-up time.}
#'  \item{"7"}{ adds both the OpenIE triples and coreference annotators to the speed 2 annotators. Can sometimes
#'              take several times longer than running them separately depending on your system resources.}
#'  \item{"8"}{ add the WikiDict, OpenIE triples, and coreference annotators to the speed 2 annotators. The
#'              speed is usually similar to speed 7, as long as you have enough memory allocated to store the
#'              WikiDict tables and still have plenty of space for the OpenIE triples (14GB+).}
#'}
#'
#' For most users, speeds 0, 2, 5, and 6 will likely be the most useful. We suggest starting at speed 2
#' and down grading to 0 if your corpus is particularly large, or upgrading the 5 or 6 if you can sacrifice
#' the slowdown. If your text is not formal written text (i.e., tweets or text messages), the speed 0
#' annotator should still work well but anything beyond that may be difficult. Semi-formal text such as e-mails
#' or transcribed speech are generally okay to run for all of the levels, though you may get errors above speed
#' 3 if the parser cannot figure out how to parse odd sections of text (the corefence and OpenIE triples annotators
#' need the parser to order to run correctly).
#'
#' @examples
#'\dontrun{
#'set_language("en")
#'}
#'
#' @export
set_language <- function(language, speed = 2) {
  language_list <- c("en", "de", "fr", "es")
  language <- match.arg(arg = language, choices = language_list)
  speed <- as.integer(speed)[1]
  if (speed < 0) stop("speed must be set to a non-negative integer")
  volatiles$language <- language

  # German models
  if (language == "de" & speed == 0) {
    set_properties("annotators", "tokenize, ssplit, pos", clear = TRUE)
    set_properties("tokenize.language", "de")
    set_properties("pos.model", "edu/stanford/nlp/models/pos-tagger/german/german-hgc.tagger")
  }
  if (language == "de" & speed == 1) {
    set_properties("annotators", "tokenize, ssplit, pos, parse", clear = TRUE)
    set_properties("tokenize.language", "de")
    set_properties("pos.model", "edu/stanford/nlp/models/pos-tagger/german/german-hgc.tagger")
    set_properties("parse.model", "edu/stanford/nlp/models/lexparser/germanFactored.ser.gz")
  }
  if (language == "de" & speed >= 2) {
    set_properties("annotators", "tokenize, ssplit, pos, ner, parse", clear = TRUE)
    set_properties("tokenize.language", "de")
    set_properties("pos.model", "edu/stanford/nlp/models/pos-tagger/german/german-hgc.tagger")
    set_properties("ner.model", "edu/stanford/nlp/models/ner/german.hgc_175m_600.crf.ser.gz")
    set_properties("ner.applyNumericClassifiers", "false")
    set_properties("ner.useSUTime", "false")
    set_properties("parse.model", "edu/stanford/nlp/models/lexparser/germanFactored.ser.gz")
  }

  # English models
  if (language == "en" & speed == 0) {
    set_properties("annotators", "tokenize, ssplit, pos, lemma", clear = TRUE)
  }
  if (language == "en" & speed == 1) {
    set_properties("annotators", "tokenize, ssplit, pos, lemma, parse, depparse, sentiment", clear = TRUE)
    set_properties("parse.model", "edu/stanford/nlp/models/srparser/englishSR.ser.gz")
  }
  if (language == "en" & speed == 2) {
    set_properties("annotators", "tokenize, ssplit, pos, lemma, parse, depparse, sentiment, ner, mention, entitymentions, natlog", clear = TRUE)
    set_properties("parse.model", "edu/stanford/nlp/models/srparser/englishSR.ser.gz")
  }
  if (language == "en" & speed == 3) {
    set_properties("annotators", "tokenize, ssplit, pos, lemma, parse, depparse, sentiment, ner, mention, entitymentions, natlog, entitylink", clear = TRUE)
    set_properties("parse.model", "edu/stanford/nlp/models/srparser/englishSR.ser.gz")
  }
  if (language == "en" & speed == 4) {
    set_properties("annotators", "tokenize, ssplit, pos, lemma, parse, depparse, sentiment, ner, mention, entitymentions, natlog, openie", clear = TRUE)
    set_properties("parse.model", "edu/stanford/nlp/models/srparser/englishSR.ser.gz")
  }
  if (language == "en" & speed == 5) {
    set_properties("annotators", "tokenize, ssplit, pos, lemma, parse, depparse, sentiment, ner, mention, entitymentions, natlog, coref", clear = TRUE)
    set_properties("parse.model", "edu/stanford/nlp/models/srparser/englishSR.ser.gz")
  }
  if (language == "en" & speed == 6) {
    set_properties("annotators", "tokenize, ssplit, pos, lemma, parse, depparse, sentiment, ner, mention, entitymentions, natlog, entitylink, coref", clear = TRUE)
    set_properties("parse.model", "edu/stanford/nlp/models/srparser/englishSR.ser.gz")
  }
  if (language == "en" & speed == 7) {
    set_properties("annotators", "tokenize, ssplit, pos, lemma, parse, depparse, sentiment, ner, mention, entitymentions, natlog, openie, coref", clear = TRUE)
    set_properties("parse.model", "edu/stanford/nlp/models/srparser/englishSR.ser.gz")
  }
  if (language == "en" & speed >= 8) {
    set_properties("annotators", "tokenize, ssplit, pos, lemma, parse, depparse, sentiment, ner, mention, entitymentions, natlog, entitylink, openie, coref", clear = TRUE)
    set_properties("parse.model", "edu/stanford/nlp/models/srparser/englishSR.ser.gz")
  }

  # Spanish models
  if (language == "es" & speed == 0) {
    set_properties("annotators", "tokenize, ssplit, pos", clear = TRUE)
    set_properties("tokenize.language", "es")
    set_properties("pos.model", "edu/stanford/nlp/models/pos-tagger/spanish/spanish-distsim.tagger")
  }
  if (language == "es" & speed == 1) {
    set_properties("annotators", "tokenize, ssplit, pos, parse", clear = TRUE)
    set_properties("tokenize.language", "es")
    set_properties("pos.model", "edu/stanford/nlp/models/pos-tagger/spanish/spanish-distsim.tagger")
    set_properties("parse.model", "edu/stanford/nlp/models/lexparser/spanishPCFG.ser.gz")
  }
  if (language == "es" & speed >= 2) {
    set_properties("annotators", "tokenize, ssplit, pos, ner, parse", clear = TRUE)
    set_properties("tokenize.language", "es")
    set_properties("pos.model", "edu/stanford/nlp/models/pos-tagger/spanish/spanish-distsim.tagger")
    set_properties("ner.model", "edu/stanford/nlp/models/ner/spanish.ancora.distsim.s512.crf.ser.gz")
    set_properties("ner.applyNumericClassifiers", "false")
    set_properties("ner.useSUTime", "false")
    set_properties("parse.model", "edu/stanford/nlp/models/lexparser/spanishPCFG.ser.gz")
  }

  # French models
  if (language == "fr" & speed == 0) {
    set_properties("annotators", "tokenize, ssplit, pos", clear = TRUE)
    set_properties("tokenize.language", "fr")
    set_properties("pos.model", "edu/stanford/nlp/models/pos-tagger/french/french.tagger")
  }
  if (language == "fr" & speed >= 1) {
    set_properties("annotators", "tokenize, ssplit, pos, parse", clear = TRUE)
    set_properties("tokenize.language", "fr")
    set_properties("pos.model", "edu/stanford/nlp/models/pos-tagger/french/french.tagger")
    set_properties("parse.model", "edu/stanford/nlp/models/lexparser/frenchFactored.ser.gz")
  }

  invisible(NULL)
}

