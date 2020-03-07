annotate_with_corenlp <- function(input, verbose) {

  assert(volatiles$corenlp$init, "You must initilize the backend.")
  assert(requireNamespace("reticulate"), "The reticulate package is required")

  doc <- vector("list", length(input))
  token <- vector("list", length(input))

  for (i in seq_len(nrow(input))) {
    x <- input$text[i]
    doc_id <- input$doc_id[i]

    if (stringi::stri_length(x)) {
      z <- volatiles$corenlp$obj$parseDocument(x, doc_id)
      token[[i]] <- as.data.frame(z$token, stringsAsFactors=FALSE)
    }

    if (verbose > 0)
    {
      if ((i %% verbose) == 0)
      {
        cmsg(verbose, "Processed document %d of %d\n", i, nrow(input))
      }
    }
  }

  anno <- list()
  if (!all(unlist(lapply(token, is.null))))
  {
    anno$token <- structure(do.call("rbind", token),
                            class = c("tbl_df", "tbl", "data.frame"))
  }
  anno$document <- input[,!(names(input) == "text"),drop=FALSE]

  return(anno)
}
