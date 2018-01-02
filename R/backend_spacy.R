#' Interface for initializing the spacy backend
#'
#' This function must be run before annotating text with
#' the spacy backend. It sets the properties for the
#' spacy engine and loads the file using the R to Python
#' interface provided by reticulate.
#'
#' @param model_name   string giving the model name for the Python/spacy
#'                     backend. Defaults to "en_core_web_sm" (English) if
#'                     set to NULL.
#' @param entity_flag  boolean. Should named entities be identified.
#' @param vector_flag  boolean. Should word vectors be computed and saved.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @examples
#'\dontrun{
#'cnlp_init_spacy(vector_flag = TRUE)
#'}
#'
#' @export
cnlp_init_spacy <- function(model_name = NULL, entity_flag = TRUE,
                            vector_flag = FALSE) {
  if (is.null(model_name))
    model_name <- "en_core_web_sm"

  volatiles$spacy$model_name  <- model_name
  volatiles$spacy$entity_flag <- entity_flag
  volatiles$spacy$vector_flag <- vector_flag

  invisible(init_spacy_backend())
}


init_spacy_backend <- function() {

  if (!requireNamespace("reticulate")) {
    stop("The reticulate package is required to use the spacy backend.")
  }

  spacy_check()

  output_loc <- system.file("py", package="cleanNLP")
  volatiles$spacy$py_file <- reticulate::py_run_file(file.path(output_loc,
                                                     "load_spacy.py"))
  model_name <- volatiles$spacy$model_name

  temp <- tryCatch({
    volatiles$spacy$spacyObj <-
      volatiles$spacy$py_file$spacyCleanNLP(model_name)
  }, error = function(e) {
    stop(sprintf("The model name '%s' cannot be found.\n", model_name),
         sprintf("You can generally download models using the following\n"),
         sprintf("command in a terminal:\n\n"),
         sprintf("   python -m spacy download %s\n\n", model_name),
         sprintf("See the spacy documentation <https://spacy.io>"),
         sprintf("for more help."))
  })

  volatiles$spacy$spacyObj$setEntityFlag(volatiles$spacy$entity_flag)
  volatiles$spacy$spacyObj$setVectorFlag(volatiles$spacy$vector_flag)

  gc() # manually garbage collect in case we just threw
       # away a large python object; it may look small to
       # R but the spacy pipeline is relatively large

  volatiles$spacy$init <- TRUE
  volatiles$model_init_last <- "spacy"

  return(NULL)
}

spacy_check <- function() {

  error_flag <- 0L

  if(!reticulate::py_available(initialize = TRUE)) {

    error_flag <- 1L
    msg <- c("Python not available", "\n",
            "See reticulate::use_python() to set python path, ", "\n",
            "then retry")

  } else if(!reticulate::py_module_available("spacy")) {

    error_flag <- 2L
    py_path <- reticulate::py_config()$python
    msg <-
          c("The spacy module is not available from the Python\n",
            "executable located here:",
            "\n\n   ",
            py_path, "\n\n",
            "This can be installed with pip by running the following:\n",
            "\n  pip install spacy\n\n",
            "If you believe it is already installed, you may be linking \n",
            "to the wrong version of Python. See reticulate::use_python()\n",
            "to set the Python path, then retry. You may need to restart\n",
            "R before use_python takes effect.")

  }

  if (error_flag > 0L)
    stop(msg, call. = FALSE)
  else
    invisible(error_flag)

}

annotate_with_spacy <- function(input, as_strings) {

  if (!volatiles$spacy$init) {
    stop("You must initialize spacy with: init_spacy_backend()")
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

  # reticulate is used as a bridge to Python
  if (!requireNamespace("reticulate")) {
    stop("The reticulate package is required to use the spacy backend.")
  }

  # set parameters within the python class and then
  # run over the input
  output_loc <- system.file("py", package="cleanNLP")
  if (length(input) <= 1)
    input <- list(input)
  volatiles$spacy$spacyObj$setOutputPath(output_dir)
  volatiles$spacy$spacyObj$processFiles(input)

  # read in the output as an R object
  out <- cnlp_read_csv(output_dir)

  # make a few minor adjustments to document table
  out$document$language <- volatiles$spacy$model_name
  if (!as_strings) {
    out$document$uri <- input
  }

  return(out)
}



