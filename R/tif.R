#' One Table Summary of an Annotation Object
#'
#' This function pulls together several tables to provide
#' a one table, denormalized version of the annotation object.
#'
#' @param annotation    an annotation object
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @export
cnlp_get_tif <- function(annotation) {

  cnlp_get_token(annotation, include_root = FALSE,
            combine = TRUE, remove_na = TRUE,
            spaces = TRUE)

}


