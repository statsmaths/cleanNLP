#' Interface for initializing the corenlp backend
#'
#' This function must be run before annotating text with
#' the corenlp backend. It sets the properties for the
#' corenlp engine and loads the file using rJava
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
#' @param lib_location   a string giving the location of the corenlp java
#'                       files. This should point to a directory which
#'                       contains, for example the file
#'                       "stanford-corenlp-*.jar",
#'                       where "*" is the version number. If missing, the
#'                       function will try to find the library in the
#'                       environment variable corenlp_HOME, and otherwise
#'                       will fail. (Java model only)
#' @param mem            a string giving the amount of memory to be assigned
#'                       to the rJava engine. For example, "6g" assigned 6
#'                       gigabytes of memory. At least 2 gigabytes are
#'                       recommended at a minimum for running the corenlp
#'                       package. On a 32bit machine, where this is not
#'                       possible, setting "1800m" may also work. This
#'                       option will only have an effect the first
#'                       time \code{init_backend} is called for the corenlp
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
#'cnlp_init_corenlp("en")
#'}
#'
#' @export
cnlp_init_corenlp <- function(language, anno_level = 2, lib_location = NULL,
                         mem = "6g", verbose = FALSE) {

  if (missing(language)) language <- "en"
  language_list <- c("en", "de", "fr", "es", "ar", "zh")
  language <- match.arg(arg = language, choices = language_list)
  anno_level <- as.integer(anno_level)[1]
  if (anno_level < 0)
    stop("anno_level must be set to a non-negative integer")
  if (is.null(lib_location))
    lib_location <- file.path(system.file("extdata", package="cleanNLP"),
                    "/stanford-corenlp-full-2018-10-05")

  # set properties that are not "corenlp" properties
  volatiles$corenlp$language <- language
  volatiles$corenlp$lib_location <- lib_location
  volatiles$corenlp$mem <- mem
  volatiles$corenlp$verbose <- verbose

  # German models
  if (language == "de" & anno_level == 0) {
    setup_corenlp_backend_raw("annotators", "tokenize, ssplit, pos, lemma",
        clear = TRUE)
    setup_corenlp_backend_raw("tokenize.language", "de")
    setup_corenlp_backend_raw("pos.model",
      "edu/stanford/nlp/models/pos-tagger/german/german-hgc.tagger")
  }
  if (language == "de" & anno_level == 1) {
    setup_corenlp_backend_raw("annotators",
      "tokenize, ssplit, pos, lemma, parse, depparse", clear = TRUE)
    setup_corenlp_backend_raw("tokenize.language", "de")
    setup_corenlp_backend_raw("pos.model",
      "edu/stanford/nlp/models/pos-tagger/german/german-hgc.tagger")
    setup_corenlp_backend_raw("parse.model",
      "edu/stanford/nlp/models/lexparser/germanFactored.ser.gz")
  }
  if (language == "de" & anno_level >= 2) {
    setup_corenlp_backend_raw("annotators",
      "tokenize, ssplit, pos, lemma, ner, parse, depparse", clear = TRUE)
    setup_corenlp_backend_raw("tokenize.language", "de")
    setup_corenlp_backend_raw("pos.model",
      "edu/stanford/nlp/models/pos-tagger/german/german-hgc.tagger")
    setup_corenlp_backend_raw("ner.model",
      "edu/stanford/nlp/models/ner/german.conll.hgc_175m_600.crf.ser.gz")
    setup_corenlp_backend_raw("ner.applyNumericClassifiers", "false")
    setup_corenlp_backend_raw("ner.useSUTime", "false")
    setup_corenlp_backend_raw("parse.model",
      "edu/stanford/nlp/models/lexparser/germanFactored.ser.gz")
  }

  # English models
  if (language == "en" & anno_level == 0) {
    setup_corenlp_backend_raw("annotators",
      "tokenize, ssplit, pos, lemma", clear = TRUE)
  }
  if (language == "en" & anno_level == 1) {
    setup_corenlp_backend_raw("annotators",
      "tokenize, ssplit, pos, lemma, parse, depparse, sentiment",
      clear = TRUE)
    #.setup_corenlp_backend_raw("parse.model",
    #  "edu/stanford/nlp/models/srparser/englishSR.ser.gz")
  }
  if (language == "en" & anno_level == 2) {
    string <- paste("tokenize, ssplit, pos, lemma, parse, depparse,",
                    "sentiment, ner, entitymentions, natlog",
                    collapse = "")
    setup_corenlp_backend_raw("annotators", string, clear = TRUE)
    #.setup_corenlp_backend_raw("parse.model",
    #  "edu/stanford/nlp/models/srparser/englishSR.ser.gz")
  }
  if (language == "en" & anno_level >= 3) {
    string <- paste("tokenize, ssplit, pos, lemma, parse, depparse,",
                    " sentiment, ner, entitymentions, natlog, coref",
                    collapse = "")
    setup_corenlp_backend_raw("annotators", string, clear = TRUE)
    #.setup_corenlp_backend_raw("parse.model",
    #  "edu/stanford/nlp/models/srparser/englishSR.ser.gz")
  }

  # Spanish models
  if (language == "es" & anno_level == 0) {
    setup_corenlp_backend_raw("annotators", "tokenize, ssplit, pos, lemma",
      clear = TRUE)
    setup_corenlp_backend_raw("tokenize.language", "es")
    setup_corenlp_backend_raw("pos.model",
      "edu/stanford/nlp/models/pos-tagger/spanish/spanish-distsim.tagger")
  }
  if (language == "es" & anno_level == 1) {
    setup_corenlp_backend_raw("annotators",
      "tokenize, ssplit, pos, lemma, parse, depparse", clear = TRUE)
    setup_corenlp_backend_raw("tokenize.language", "es")
    setup_corenlp_backend_raw("pos.model",
      "edu/stanford/nlp/models/pos-tagger/spanish/spanish-distsim.tagger")
    setup_corenlp_backend_raw("parse.model",
      "edu/stanford/nlp/models/lexparser/spanishPCFG.ser.gz")
  }
  if (language == "es" & anno_level >= 2) {
    setup_corenlp_backend_raw("annotators",
      "tokenize, ssplit, pos, lemma, ner, parse, depparse", clear = TRUE)
    setup_corenlp_backend_raw("tokenize.language", "es")
    setup_corenlp_backend_raw("pos.model",
      "edu/stanford/nlp/models/pos-tagger/spanish/spanish-distsim.tagger")
    setup_corenlp_backend_raw("ner.model",
      "edu/stanford/nlp/models/ner/spanish.ancora.distsim.s512.crf.ser.gz")
    setup_corenlp_backend_raw("ner.applyNumericClassifiers", "false")
    setup_corenlp_backend_raw("ner.useSUTime", "false")
    setup_corenlp_backend_raw("parse.model",
      "edu/stanford/nlp/models/lexparser/spanishPCFG.ser.gz")
  }

  # French models
  if (language == "fr" & anno_level == 0) {
    setup_corenlp_backend_raw("annotators",
      "tokenize, ssplit, pos, lemma", clear = TRUE)
    setup_corenlp_backend_raw("tokenize.language", "fr")
    setup_corenlp_backend_raw("pos.model",
      "edu/stanford/nlp/models/pos-tagger/french/french.tagger")
  }
  if (language == "fr" & anno_level >= 1) {
    setup_corenlp_backend_raw("annotators",
      "tokenize, ssplit, pos, lemma, parse, depparse", clear = TRUE)
    setup_corenlp_backend_raw("tokenize.language", "fr")
    setup_corenlp_backend_raw("pos.model",
      "edu/stanford/nlp/models/pos-tagger/french/french.tagger")
    setup_corenlp_backend_raw("parse.model",
      "edu/stanford/nlp/models/lexparser/frenchFactored.ser.gz")
  }

  # Arabic models
  if (language == "ar" & anno_level >= 0) {
    setup_corenlp_backend_raw("annotators",
      "segment, tokenize, ssplit, pos, parse", clear = TRUE)
    setup_corenlp_backend_raw("customAnnotatorClass.segment",
      "edu.stanford.nlp.pipeline.ArabicSegmenterAnnotator")
    setup_corenlp_backend_raw("segment.model",
      "edu/stanford/nlp/models/segmenter/arabic/arabic-segmenter-atb+bn+arztrain.ser.gz")
    setup_corenlp_backend_raw("ssplit.boundaryTokenRegex",
      "[.]|[!?]+|[!\\u061F]+")
    setup_corenlp_backend_raw("pos.model",
      "edu/stanford/nlp/models/pos-tagger/arabic/arabic.tagger")
    setup_corenlp_backend_raw("parse.model",
      "edu/stanford/nlp/models/lexparser/arabicFactored.ser.gz")
  }

  # Chinese models
  if (language == "zh" & anno_level == 0) {
    setup_corenlp_backend_raw("annotators",
      "tokenize, ssplit, pos, lemma", clear = TRUE)

    setup_corenlp_backend_raw("tokenize.language",
      "zh")
    setup_corenlp_backend_raw("segment.model",
      "edu/stanford/nlp/models/segmenter/chinese/ctb.gz")
    setup_corenlp_backend_raw("segment.sighanCorporaDict",
      "edu/stanford/nlp/models/segmenter/chinese")
    setup_corenlp_backend_raw("segment.serDictionary",
      "edu/stanford/nlp/models/segmenter/chinese/dict-chris6.ser.gz")
    setup_corenlp_backend_raw("segment.sighanPostProcessing",
      "true")
    setup_corenlp_backend_raw("ssplit.boundaryTokenRegex",
      "[.\u3002]|[!?\uFF01\uFF1F]+")
    setup_corenlp_backend_raw("pos.model",
      "edu/stanford/nlp/models/pos-tagger/chinese-distsim/chinese-distsim.tagger")
  }

  if (language == "zh" & anno_level == 1) {
    setup_corenlp_backend_raw("annotators",
      "tokenize, ssplit, pos, lemma, ner, parse", clear = TRUE)

    setup_corenlp_backend_raw("tokenize.language",
      "zh")
    setup_corenlp_backend_raw("segment.model",
      "edu/stanford/nlp/models/segmenter/chinese/ctb.gz")
    setup_corenlp_backend_raw("segment.sighanCorporaDict",
      "edu/stanford/nlp/models/segmenter/chinese")
    setup_corenlp_backend_raw("segment.serDictionary",
      "edu/stanford/nlp/models/segmenter/chinese/dict-chris6.ser.gz")
    setup_corenlp_backend_raw("segment.sighanPostProcessing",
      "true")
    setup_corenlp_backend_raw("ssplit.boundaryTokenRegex",
      "[.\u3002]|[!?\uFF01\uFF1F]+")
    setup_corenlp_backend_raw("pos.model",
      "edu/stanford/nlp/models/pos-tagger/chinese-distsim/chinese-distsim.tagger")
    setup_corenlp_backend_raw("ner.language",
      "chinese")
    setup_corenlp_backend_raw("ner.model",
      "edu/stanford/nlp/models/ner/chinese.misc.distsim.crf.ser.gz")
    setup_corenlp_backend_raw("ner.applyNumericClassifiers",
      "true")
    setup_corenlp_backend_raw("ner.useSUTime",
      "false")
    setup_corenlp_backend_raw("regexner.mapping",
      "edu/stanford/nlp/models/kbp/cn_regexner_mapping.tab")
    setup_corenlp_backend_raw("regexner.validpospattern",
      "^(NR|NN|JJ).*")
    setup_corenlp_backend_raw("regexner.ignorecase",
      "true")
    setup_corenlp_backend_raw("regexner.noDefaultOverwriteLabels",
      "CITY")
    setup_corenlp_backend_raw("parse.model",
      "edu/stanford/nlp/models/srparser/chineseSR.ser.gz")
    setup_corenlp_backend_raw("depparse.model",
      "edu/stanford/nlp/models/parser/nndep/UD_Chinese.gz")
    setup_corenlp_backend_raw("depparse.language",
      "chinese")
  }

  if (language == "zh" & anno_level >= 2) {
    setup_corenlp_backend_raw("annotators",
      "tokenize, ssplit, pos, lemma, ner, parse, coref", clear = TRUE)

    setup_corenlp_backend_raw("tokenize.language",
      "zh")
    setup_corenlp_backend_raw("segment.model",
      "edu/stanford/nlp/models/segmenter/chinese/ctb.gz")
    setup_corenlp_backend_raw("segment.sighanCorporaDict",
      "edu/stanford/nlp/models/segmenter/chinese")
    setup_corenlp_backend_raw("segment.serDictionary",
      "edu/stanford/nlp/models/segmenter/chinese/dict-chris6.ser.gz")
    setup_corenlp_backend_raw("segment.sighanPostProcessing",
      "true")
    setup_corenlp_backend_raw("ssplit.boundaryTokenRegex",
      "[.\u3002]|[!?\uFF01\uFF1F]+")
    setup_corenlp_backend_raw("pos.model",
      "edu/stanford/nlp/models/pos-tagger/chinese-distsim/chinese-distsim.tagger")
    setup_corenlp_backend_raw("ner.language",
      "chinese")
    setup_corenlp_backend_raw("ner.model",
      "edu/stanford/nlp/models/ner/chinese.misc.distsim.crf.ser.gz")
    setup_corenlp_backend_raw("ner.applyNumericClassifiers",
      "true")
    setup_corenlp_backend_raw("ner.useSUTime",
      "false")
    setup_corenlp_backend_raw("regexner.mapping",
      "edu/stanford/nlp/models/kbp/cn_regexner_mapping.tab")
    setup_corenlp_backend_raw("regexner.validpospattern",
      "^(NR|NN|JJ).*")
    setup_corenlp_backend_raw("regexner.ignorecase",
      "true")
    setup_corenlp_backend_raw("regexner.noDefaultOverwriteLabels",
      "CITY")
    setup_corenlp_backend_raw("parse.model",
      "edu/stanford/nlp/models/srparser/chineseSR.ser.gz")
    setup_corenlp_backend_raw("depparse.model",
      "edu/stanford/nlp/models/parser/nndep/UD_Chinese.gz")
    setup_corenlp_backend_raw("depparse.language",
      "chinese")
    setup_corenlp_backend_raw("coref.sieves",
      "ChineseHeadMatch, ExactStringMatch, PreciseConstructs, StrictHeadMatch1, StrictHeadMatch2, StrictHeadMatch3, StrictHeadMatch4, PronounMatch")
    setup_corenlp_backend_raw("coref.input.type",
      "raw")
    setup_corenlp_backend_raw("coref.postprocessing",
      "true")
    setup_corenlp_backend_raw("coref.calculateFeatureImportance",
      "false")
    setup_corenlp_backend_raw("coref.useConstituencyTree",
      "true")
    setup_corenlp_backend_raw("coref.useSemantics",
      "false")
    setup_corenlp_backend_raw("coref.algorithm",
      "hybrid")
    setup_corenlp_backend_raw("coref.path.word2vec",
      "")
    setup_corenlp_backend_raw("coref.language",
      "zh")
    setup_corenlp_backend_raw("coref.defaultPronounAgreement",
      "true")
    setup_corenlp_backend_raw("coref.zh.dict",
      "edu/stanford/nlp/models/dcoref/zh-attributes.txt.gz")
    setup_corenlp_backend_raw("coref.defaultPronounAgreement",
      "true")
    setup_corenlp_backend_raw("coref.zh.dict",
      "edu/stanford/nlp/models/dcoref/zh-attributes.txt.gz")
    setup_corenlp_backend_raw("coref.print.md.log",
      "false")
    setup_corenlp_backend_raw("coref.md.type",
      "RULE")
    setup_corenlp_backend_raw("coref.md.liberalChineseMD",
      "false")
    setup_corenlp_backend_raw("kbp.semgrex",
      "edu/stanford/nlp/models/kbp/chinese/semgrex")
    setup_corenlp_backend_raw("kbp.tokensregex",
      "edu/stanford/nlp/models/kbp/chinese/tokensregex")
    setup_corenlp_backend_raw("kbp.model",
      "none")
    setup_corenlp_backend_raw("entitylink.wikidict",
      "edu/stanford/nlp/models/kbp/wikidict_chinese.tsv.gz")
  }

  invisible(init_corenlp_backend())
}

setup_corenlp_backend_raw <- function(keys, values, clear = FALSE) {
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
      prop <- readRDS(fin)
    else
      prop <- list()
  } else prop <- list()

  # insert new keys and values
  for (i in seq_along(keys))
    prop[[keys[i]]] <- values[i]

  # save new parameter file
  saveRDS(prop, file.path(system.file("extdata",
    package="cleanNLP"), "properties.rds"))
}

init_corenlp_backend <- function() {

  if (!requireNamespace("rJava")) {
    stop("The rJava package is required to use the corenlp backend")
  }

  # Start java engine, if not already done
  options(java.parameters = paste0("-Xmx", volatiles$corenlp$mem))
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
    setup_corenlp_backend_raw("en")
  }
  properties <- readRDS(fp)
  keys <- names(properties)
  values <- as.character(properties)
  lang <- volatiles$corenlp$language

  # Find location of the corenlp Libraries
  if (!file.exists(volatiles$corenlp$lib_location)) {
    stop("Please run cnlp_download_corenlp() in order to ",
      "install required jar files.")
  } else {
    path <- Sys.glob(file.path(volatiles$corenlp$lib_location,
      "*.jar"))
  }

  # Start java engine, if not already done, and add to classpath
  rJava::.jaddClassPath(file.path(system.file("extdata",
    package = "cleanNLP"), "cleanNLP-0.1.jar"))
  rJava::.jaddClassPath(path)

  # Determine if the corenlp files have been loaded correctly
  jar_files <- basename(rJava::.jclassPath())
  if (!("stanford-corenlp-3.9.2.jar" %in% jar_files))
    warning("The Stanford corenlp (3.7.2) files were not found ",
            "as expected. Proceed with caution.")
  if (lang == "en" && !("stanford-english-corenlp-2018-10-05-models.jar"
    %in% jar_files))
    warning("English model file has not been downloaded to lib_location.",
            "Proceed with caution.")
  if (lang == "fr" && !("stanford-french-corenlp-2018-10-05-models.jar"
    %in% jar_files))
    warning("French model file has not been downloaded to lib_location.",
      "Proceed with caution.")
  if (lang == "de" && !("stanford-german-corenlp-2018-10-05-models.jar"
    %in% jar_files))
    warning("German model file has not been downloaded to lib_location.",
      "Proceed with caution.")
  if (lang == "es" && !("stanford-spanish-corenlp-2018-10-05-models.jar"
    %in% jar_files))
    warning("Spanish model file has not been downloaded to lib_location.",
      "Proceed with caution.")

  # If running for the second time, reset the annotator pool
  # We create a second one anyway, but it's good to release the
  # memory explicitly.
  if (!is.null(volatiles$corenlp$corenlp))
    rJava::.jcall(volatiles$corenlp$corenlp, "V", "clearAnnotatorPool")

  # Apply properties to a java properties object
  prop <- rJava::.jnew("java.util.Properties")
  for (i in seq_along(keys))
    prop$setProperty(keys[i], values[i])

  # Load the NLP pipeline (quietly, if desired)
  if (!volatiles$corenlp$verbose) {
    err <- rJava::.jfield("java/lang/System", , "err")
    rJava::.jcall("java/lang/System", "V", "setErr",
      rJava::.jnew("java/io/PrintStream",
      rJava::.jcast(rJava::.jnew("java/io/ByteArrayOutputStream"),
        "java/io/OutputStream")))
  } else {
    message("Loading NLP pipeline.\n(This may take several minutes.",
      "Please be patient.)")
  }

  volatiles$corenlp$corenlp <-
    rJava::.jnew("edu.stanford.nlp.pipeline.StanfordCoreNLP", prop)
  volatiles$corenlp$AnnotationProcessor <-
    rJava::.jnew("edu.richmond.nlp.AnnotationProcessor")
  if (!volatiles$corenlp$verbose)
    rJava::.jcall("java/lang/System", "V", "setErr", err)

  gc() # manually garbage collect in case we just threw
       # away a large Java object; it may look small to
       # R (just a pointer) but the corenlp pipeline is
       # very large

  volatiles$corenlp$init <- TRUE
  volatiles$model_init_last <- "corenlp"

  invisible(NULL)
}


annotate_with_corenlp <- function(input, as_strings) {

  if (!volatiles$corenlp$init) {
    stop("You must initialize corenlp with: cnlp_init_corenlp()")
  }

  output_dir <- tempfile()   # yes, we want tempfile and not tempdir; the
                             # latter points to a static directory that is
                             # persistent through the R session; tempfile()
                             # gives a random path *within* that directory;
                             # we are free to treat it as a directory rather
                             # than a file.

  # the spacy module saves the results on disk, so we need to have
  # a place to put the output
  dir.create(output_dir, FALSE, TRUE)

  # have to follow python file naming rules; causes a problem
  # in windows if we use the default R values
  output_dir <- file.path(Sys.glob(output_dir), "/")
  output_dir <- gsub("\\", "/", output_dir, fixed = TRUE)

  # for now, we will only work with strings stored on disk, so
  # write strings to disk (NOTE: yes this is silly and needs to
  # be modified)
  if (as_strings) {
    new_input <- NULL
    for (i in seq_along(input)) {
      this_file <- tempfile()
      new_input <- c(new_input, this_file)
      writeLines(input[i], this_file)
    }
    input <- new_input
  }

  # find the input values; if none are found return an error
  input <- Sys.glob(input)
  if (length(input) == 0) {
    stop("No valid files found.")
  }

  # rJava is used as a bridge to Python
  if (!requireNamespace("rJava")) {
    stop("The rJava package is required to use the coreNLP backend.")
  }

  # set parameters within the Java class
  rJava::.jcall(volatiles$corenlp$AnnotationProcessor, "V",
                "setOutputPath", output_dir)
  rJava::.jcall(volatiles$corenlp$AnnotationProcessor, "V",
                "setLanguage", volatiles$corenlp$language)

  # run the AnnotationProcessor method "processFiles"
  rJava::.jcall(volatiles$corenlp$AnnotationProcessor,
                "V",
                "processFiles",
                rJava::.jarray(input),
                volatiles$corenlp$corenlp)

  # read in the output as an R object, if desired; otherwise
  # just return a path to the files; the latter is useful if
  # the output is very large
  out <- cnlp_read_csv(output_dir)

  return(out)
}


