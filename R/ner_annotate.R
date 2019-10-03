#' Run the annotation pipeline on a set of documents to extract entities
#'
#' Runs the entity detection algorithms from CoreNLP using either the rJava.
#' It initializes the CoreNLP Java object with the NER annotation parameters
#' and a path to a temp file that is used for processing using \code{initCoreNLPNER}.
#' The function returns a \code{data.frame} showing the location in the document 
#' where the entity occurs andthe entity type. If no entities are detected for a document 
#' then a row of NA values is returned.
#'
#' @param input.file a character string showing the path to the file to be processed
#' @return a data.frame with the details of the detected entities.
#' @importFrom jsonlite fromJSON
#' @importFrom rJava .jcall
#'
#' @export
NERAnnotate <- function(input.file) {
  
  if(!volatiles$corenlp$init)
    stop("Java CoreNLP not initialized. Named Entity Recognition cannot be executed.")
  
  .jcall(volatiles$corenlp$corenlp, "V", "run")
  
  output <- fromJSON(paste0(input.file, ".json"))
  relevant.cols = c("text", "ner")
  ner.mentions = output$sentences$entitymentions
  response = sapply(ner.mentions, function(x) nrow(x))
  if(all(sapply(response, is.null))) {
    out <- data.frame(id = character(), entity = character(), entity.type = character())
  } else {
    response = rep(1:length(ner.mentions), response)
    ner.mentions = lapply(ner.mentions, function(x) {if(nrow(x) != 0) {
      subset(x, select = relevant.cols)
    }})
    # Remove the NULL list elements
    ner.mentions <- Filter(Negate(is.null), ner.mentions)
    out = cbind(response, do.call(rbind.data.frame, ner.mentions))
    names(out) <- c("id", "entity", "entity.type")
  }
  out
}