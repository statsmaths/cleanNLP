#' Run the annotation pipeline on a set of documents to extract entities
#'
#' Runs the entity detection algorithms from CoreNLP using CoreNLP java library via rJava.
#' It expects the CoreNLP java object to already be initialised with rJava with a call to 
#' \code{cnlp_init_corenlp_custom} with the appropriate annotators setup for named entity
#' recognition and a path to an input file that has the input text separately by new lines.
#' The input file must have Unix style line endings or will cause the CoreNLP java call to crash
#' with a null pointer exception. The function returns a \code{data.frame} showing the location
#' in the document whre each entity occurs and the entity type. If no entities are detected for a
#' document then an empty data.frame with no rows is returned.
#'
#' @param input.file a character string showing the path to the file to be processed. The file should
#'    have text with Unix style line endings (will throw Nullpointer exception if not)
#' @param entity.mentions.only Logical to specify if only entity mention output from CoreNLP is used
#'    in the extraction. If TRUE, this will extract personal pronouns as well as standard entities.
#'    The benefit of the entity.mention output is it groups words that are from the same entity. E.g.
#'    'John Smith' is a single person entity and 'New York City' is a single location entity. The 
#'    entity mention output also classifies personal pronouns as entities and will be extracted.
#'    If FALSE, the entity.mentions output from CoreNLP is validated against the CoreNLP token output.
#'    The token output also identifies entities on the single word level and it doesn't classify personal
#'    pronouns as entities. The net effect if set to FALSE is that entity mentions are extracted but
#'    personal pronouns and other potential entity mentions that are not entities on the token level
#'    are not extracted.
#' @return data.frame with the details of the detected entities. The output data.frame has three
#'    columns. \itemize{
#'        \item \code{id} integer: the row index of the input file that has an extracted entity.
#'        \item \code{entity} character: The extracted entity word (e.g. William)
#'        \item \code{entity.tyoe} character: The entity type of the extracted entity (e.g. Person)
#'    }
#' @importFrom jsonlite fromJSON
#' @importFrom rJava .jcall
#' @examples
#' \dontrun{
#' simple.input.test <- c("John is a person", "Google is a company", "This is nothing")
#' input.file <- tempfile()
#' file <- file(input.file, "wb") # need linux style line endings
#' writeLines(simple.input.test, con = file)
#' close(file)
#' keys <- c("ssplit.eolonly", "annotators", "outputFormat", "file", "outputDirectory")
#' values <- c("true", "tokenize,ssplit,pos,lemma,ner", "json", input.file, dirname(input.file))
#' 
#' cnlp_init_corenlp_custom(language = "en", mem = "2g", keys = keys, values = values)
#' simple.output <- NERAnnotate(input.file)
#' }
#' @export
NERAnnotate <- function(input.file, entity.mentions.only = FALSE) {
  
  if(!volatiles$corenlp$init)
    stop("Java CoreNLP not initialized. Named Entity Recognition cannot be executed.")
  
  .jcall(volatiles$corenlp$corenlp, "V", "run")
  
  output <- fromJSON(paste0(input.file, ".json"))
  ner.mentions = output$sentences$entitymentions
  response = sapply(ner.mentions, function(x) nrow(x))
  if(all(sapply(response, is.null))) {
    out <- data.frame(id = character(), entity = character(), entity.type = character())
  } else {
    if(!entity.mentions.only) {
      # Validate ner.mentions against the token output
      ner.mentions <- mapply(function(x, y) {
        if(nrow(y) != 0) {
          idx <- sapply(1:nrow(y), function(i) {
            z <- y[i, ]
            ind <- c(z$tokenBegin + 1, z$tokenEnd)
            all(x[ind, ]$ner != "O")})
          if(any(idx))
          {
            y <- y[idx, ]
          } else
          {
            return(data.frame())
          }
        }
        y
      }, x = output$sentences$tokens, y = ner.mentions)
      response <- sapply(ner.mentions, function(x) nrow(x))
      # Check if filtered ner is not empty
      if(all(sapply(response, is.null))) {
        return(data.frame(id = character(), entity = character(), entity.type = character())) 
      }
    }
    response <- rep(1:length(ner.mentions), response)
    ner.mentions = lapply(ner.mentions, function(x) {
      if(nrow(x) != 0) {
        subset(x, select = c("text", "ner"))
    }})
    # Remove the NULL list elements
    ner.mentions <- Filter(Negate(is.null), ner.mentions)
    out = cbind(response, do.call(rbind.data.frame, ner.mentions))
    names(out) <- c("id", "entity", "entity.type")
  }
  out
}
