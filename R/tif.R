#' Wrapper for TIF Compliant Usage
#'
#' Runs annotations over a corpus object and returns results as
#' an annotation object. See parameters below for the specific
#' input format For more control, see the function
#' \code{\link{cnlp_annotate}}.
#'
#' @param input          a data frame representing a tif corpus object.
#'                       The first column should contain a unique document
#'                       identifier and the second should contain the raw
#'                       text.
#'
#' @return  an annotation object
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @examples
#'\dontrun{
#'annotation <- cnlp_annotate_tif(tif_data_frame)
#'}
#'
#' @export
cnlp_annotate_tif <- function(input) {

  if (!is.data.frame(input))
    stop("The input should be a data frame")
  if (ncol(input) <= 1)
    stop("The input should have at least two columns")
  if (class(input[[2]]) != "character")
    stop("The second column of the input should contain a character vector")

  names(input)[1:2] <- c("doc_id", "text")

  anno <- cnlp_annotate(input$text, doc_ids = input$doc_id,
                        as_strings = TRUE)

  return(anno)
}

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


