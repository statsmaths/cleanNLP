annotate_with_udpipe <- function(input, verbose) {

  assert(volatiles$udpipe$init, "You must initilize the backend.")
  assert(requireNamespace("udpipe"), "The udpipe package is required")

  doc <- vector("list", length(input))
  token <- vector("list", length(input))

  for (i in seq_len(nrow(input))) {
    x <- input$text[i]
    doc_id <- input$id[i]

    anno <- as.data.frame(
      udpipe::udpipe_annotate(volatiles$udpipe$model_obj, x)
    )
    anno$token_id <- as.numeric(anno$token_id)
    anno <- anno[!is.na(anno$token_id),]

    token[[i]] <- data.frame(
      id=doc_id,
      sid=anno$sentence_id,
      tid=anno$token_id,
      token=anno$token,
      lemma=anno$lemma,
      upos=anno$upos,
      xpos=anno$xpos,
      feats=anno$feats,
      tid_source=anno$head_token_id,
      relation=anno$dep_rel,
      stringsAsFactors=FALSE
    )

    cmsg(verbose, "Processed document %d of %d\n", i, nrow(input))
  }

  anno <- list()
  anno$token <- structure(do.call("rbind", token),
                          class = c("tbl_df", "tbl", "data.frame"))
  anno$document <- input

  return(anno)
}
