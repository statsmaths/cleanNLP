#' Interface for initializing the tokenizers backend
#'
#' This function must be run before annotating text with
#' the tokenizers backend.
#'
#' @param locale   string giving the locale name to
#'                 pass to the stringi functions. If
#'                 \code{NULL}, the default locale is
#'                 selected
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @examples
#'\dontrun{
#'cnlp_init_tokenizers()
#'}
#'
#' @export
cnlp_init_tokenizers <- function(locale = NULL) {
  invisible(init_tokenizers_backend(locale))
}

init_tokenizers_backend <- function(locale = NULL) {

  if (!requireNamespace("stringi")) {
    stop("The stringi package is required to", # nocov
         "use this backend.")                  # nocov
  }

  if (is.null(locale)) {
    locale <- stringi::stri_locale_get()
  }

  volatiles$tokenizers$init <- TRUE
  volatiles$tokenizers$locale <- locale
  volatiles$model_init_last <- "tokenizers"

  return(NULL)
}

annotate_with_r <- function(input, as_strings) {

  if (!volatiles$tokenizers$init) {
    stop("You must initialize tokenizers with: init_tokenizers_backend()")
  }

  # stringi package is used for the annotation
  if (!requireNamespace("stringi")) {
    stop("The stringi package is required to use the tokenizers backend.")
  }

  # set the locale (reset to old value on exit)
  old_locale <- stringi::stri_locale_get()
  suppressMessages(stringi::stri_locale_set(volatiles$tokenizers$locale))
  on.exit(suppressMessages(stringi::stri_locale_set(old_locale)))

  token <- vector("list", length(input))
  doc <- vector("list", length(input))

  for (i in seq_along(input)) {
    x <- input[i]
    doc_id <- sprintf("doc%d", i)

    # add documents to the documents table
    doc[[i]] <- data.frame(id = doc_id,
                     time = format(Sys.time(), fmt = "%dZ", tz = "UTC"),
                     version = as.character(utils::packageVersion("cleanNLP")),
                     language = volatiles$tokenizers$locale,
                     uri = x,
                     stringsAsFactors = FALSE)

    # load documents if the input gives file paths
    # rather than raw text
    if (!as_strings) {
      x <- readLines(x)
      x <- iconv(x, sub = "")
      x <- paste(x, collapse = " ")
    }

    if (stringi::stri_length(x) == 0L) next

    sent <- stringi::stri_split_boundaries(x, type = "sentence",
                    skip_word_none = FALSE)
    sent <- sent[[1]]
    sent_len <- stringi::stri_length(sent)
    sent_len <- cumsum(c(0, sent_len[-length(sent_len)]))

    cid <- stringi::stri_locate_all_boundaries(sent, type = "word",
                    skip_word_none = FALSE)
    cid <- lapply(cid, function(v) v[,1])

    word <- stringi::stri_split_boundaries(sent, type = "word",
                    skip_word_none = FALSE)

    # remove white space "words"
    index <- lapply(word, stringi::stri_detect, regex = "^[ ]+$")
    cid <- mapply(function(u, v) u[!v], cid, index)
    word <- mapply(function(u, v) u[!v], word, index)

    # add ROOT to each sentence
    cid <- lapply(cid, function(v) c(NA_integer_, v))
    word <- lapply(word, function(v) c("ROOT", v))

    # create ids
    sid <- mapply(function(u, v) rep(u, length(v)), seq_along(word), word)
    tid <- mapply(function(u) seq(length(u)) - 1L, word)

    cid <- unlist(cid)
    word <- unlist(word)
    sid <- as.numeric(unlist(sid))
    tid <- as.numeric(unlist(tid))
    cid <- cid + sent_len[sid]

    if (length(word) == 0L) next

    token[[i]] <- data.frame(id = doc_id, sid = as.integer(sid),
                            tid = as.integer(tid),
                            word = word, lemma = NA_character_,
                            upos = NA_character_,
                            pos = NA_character_,
                            cid = as.integer(cid),
                            stringsAsFactors = FALSE)

  }

  anno <- empty_anno()
  anno$token <- structure(do.call("rbind", token),
                          class = c("tbl_df", "tbl", "data.frame"))
  anno$document <- structure(do.call("rbind", doc),
                          class = c("tbl_df", "tbl", "data.frame"))

  return(anno)
}
