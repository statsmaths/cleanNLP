#' Wrapper for TIF Compliant Usage
#'
#' Runs annotations over a corpus object and returns results as
#' a single data frame. See parameters below for the specific
#' input parameters. For more control, see the function
#' \code{\link{run_annotators}}.
#'
#' @param input          a data frame representing a tif corpus object.
#'                       The first column should contain a unique document
#'                       identifier and the second should contain the raw
#'                       text.
#'
#' @return  a data frame containing the parsed text, with one row for
#'          each token
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @examples
#'\dontrun{
#'annotation <- tif_annotation(tif_data_frame)
#'}
#'
#' @export
tif_annotation <- function(input) {

  if (!is.data.frame(input))
    stop("The input should be a data frame")
  if (ncol(input) <= 1)
    stop("The input should have at least two columns")
  if (class(input[[2]]) != "character")
    stop("The second column of the input should contain a character vector")

  names(input)[1:2] <- c("doc_id", "text")

  anno <- run_annotators(input$text, as_strings = TRUE)
  comb <- get_combine(anno)

  # replace document id with tif standard
  names(comb)[1] <- "doc_id"
  comb$doc_id <- input$doc_id[comb$doc_id]

  # if other columns, combine those as well
  if (ncol(input) > 2) {
    dplyr::left_join(comb, input[,-2], by = "doc_id")
  }

  return(comb)
}



