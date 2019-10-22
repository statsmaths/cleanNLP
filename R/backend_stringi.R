annotate_with_stringi <- function(input, verbose) {

  assert(volatiles$stringi$init, "You must initilize the backend.")
  assert(requireNamespace("stringi"), "The stringi package is required")

  token <- vector("list", length(input))
  doc <- vector("list", length(input))

  for (i in seq_len(nrow(input))) {
    x <- input$text[i]
    doc_id <- input$doc_id[i]

    if (stringi::stri_length(x) == 0L) next

    sent <- stringi::stri_split_boundaries(
      x,
      type = "sentence",
      skip_word_none = FALSE,
      locale=volatiles$stringi$locale
    )[[1]]

    word <- stringi::stri_split_boundaries(
      sent,
      type = "word",
      locale=volatiles$stringi$locale,
      skip_word_none = FALSE
    )

    word_leng <- lapply(word, function(v) cumsum(stringi::stri_length(v)))
    word_real <- lapply(word, stringi::stri_detect, regex = "^[ ]+$")
    word <- mapply(function(u, v) u[!v], word, word_real, SIMPLIFY=FALSE)

    # create ids
    sid <- mapply(function(u, v) rep(u, length(v)), seq_along(word), word)
    tid <- mapply(function(u) seq(length(u)) - 1L, word)

    word <- unlist(word)
    sid <- as.numeric(unlist(sid))
    tid <- as.numeric(unlist(tid))

    if (length(word) == 0L) next

    token[[i]] <- data.frame(
      doc_id = doc_id,
      sid = as.integer(sid),
      tid = as.integer(tid),
      token = word,
      lemma = stringi::stri_trans_tolower(word),
      stringsAsFactors = FALSE
    )

    cmsg(verbose, "Processed document %d of %d\n", i, nrow(input))
  }

  anno <- list()
  anno$token <- structure(do.call("rbind", token),
                          class = c("tbl_df", "tbl", "data.frame"))
  anno$document <- input[,!(names(input) == "text"),drop=FALSE]

  return(anno)
}
