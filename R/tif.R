#' One Table Summary of an Annotation Object
#'
#' This function pulls together several tables to provide
#' a one table, denormalized version of the annotation object.
#'
#' @param annotation    an annotation object
#' @param rename        logical. Should columns be renamed to match
#'                      the text interchange format.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @export
cnlp_get_tif <- function(annotation, rename=TRUE) {

  output <- cnlp_get_token(annotation, include_root = FALSE,
                           combine = TRUE, remove_na = TRUE,
                           spaces = TRUE)

  if (rename) {
    names(output)[c(1L, 4L)] <- c("doc_id", "token") 
  }
  
  return(output)
}


