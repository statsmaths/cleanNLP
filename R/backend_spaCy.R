#' Interface for initializing up the spaCy backend
#'
#' This function must be run before annotating text with
#' the tokenizers backend. It sets the properties for the
#' spaCy engine and loads the file using the R to Python
#' interface provided by reticulate.
#'
#' @param entity_flag  boolean. Should named entities be identified.
#' @param vector_flag  boolean. Should word vectors be computed and saved.
#' @param model_name   string giving the model name for the Python/spaCy
#'                     backend. Defaults to "en" (English) if NULL.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @examples
#'\dontrun{
#'init_spaCy(vector_flag = TRUE)
#'}
#'
#' @export
init_spaCy <- function(entity_flag = TRUE, vector_flag = FALSE,
                       model_name = NULL) {
  if (is.null(model_name))
    model_name <- "en"

  volatiles$spaCy$model_name  <- model_name
  volatiles$spaCy$entity_flag <- entity_flag
  volatiles$spaCy$vector_flag <- vector_flag

  invisible(.init_spaCy_backend())
}


.init_spaCy_backend <- function() {

  if (!requireNamespace("reticulate")) {
    stop("The reticulate package is required to use the spaCy backend.")
  }

  if (!reticulate::py_module_available("spacy")) {
    stop("The spaCy module must be installed in python before",
         "using as a backend.")
  }

  output_loc <- system.file("py", package="cleanNLP")
  volatiles$spaCy$py_file <- reticulate::py_run_file(file.path(output_loc,
                                                     "load_spacy.py"))
  model_name <- volatiles$spaCy$model_name

  temp <- tryCatch({
    volatiles$spaCy$SpacyObj <-
      volatiles$spaCy$py_file$SpacyCleanNLP(model_name)
  }, error = function(e) {
    stop(sprintf("The model name '%s' cannot be found.\n", model_name),
         sprintf("You can generally download models using the following\n"),
         sprintf("command in a terminal:\n\n"),
         sprintf("   python -m spacy download %s\n\n", model_name),
         sprintf("See the spaCy documentation <https://spacy.io>"),
         sprintf("for more help."))
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
