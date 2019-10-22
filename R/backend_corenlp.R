annotate_with_corenlp <- function(input, verbose) {

  assert(volatiles$corenlp$init, "You must initilize the backend.")
  assert(requireNamespace("reticulate"), "The reticulate package is required")

  doc <- vector("list", length(input))
  token <- vector("list", length(input))

  for (i in seq_len(nrow(input))) {
    x <- input$text[i]
    doc_id <- input$doc_id[i]

    z <- volatiles$corenlp$obj$parseDocument(x, doc_id)
    token[[i]] <- as.data.frame(z$token, stringsAsFactors=FALSE)

    cmsg(verbose, "Processed document %d of %d\n", i, nrow(input))
  }

  anno <- list()
  anno$token <- structure(do.call("rbind", token),
                          class = c("tbl_df", "tbl", "data.frame"))
  anno$document <- input[,!(names(input) == "text"),drop=FALSE]

  return(anno)
}
