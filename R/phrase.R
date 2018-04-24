#' Run the annotation pipeline on a set of documents
#'
#' Runs the clean_nlp annotators over a given corpus of text
#' using either the R, Java, or Python backend. The details for
#' which annotators to run and how to run them are specified
#' by using one of: \code{\link{cnlp_init_tokenizers}},
#' \code{\link{cnlp_init_spacy}}, \code{\link{cnlp_init_udpipe}},
#' or \code{\link{cnlp_init_corenlp}}.
#'
#' @param input          either a vector of file names to parse, a
#'                       character vector with one document in each
#'                       element, or a data frame. If a data frame,
#'                       specify what column names contain the text and
#'                       (optionally) document ids



#' @export
cnlp_utils_phrase <- function(object,
                              type = c("noun_phrase", "prop_nouns",
                                       "verb_phrase", "modifiers"),
                              format = c("collapse", "append", "full"))
{



}


