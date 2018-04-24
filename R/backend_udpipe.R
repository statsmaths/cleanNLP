#' Interface for initializing the udpipe backend
#'
#' This function must be run before annotating text with
#' the udpipe backend. It will parse in English by default,
#' but you can load other models as well.
#'
#' @param model_name   string giving the model namel.
#'                     Defaults to "english" if NULL.
#'                     Ignored if \code{model_path} is not NULL.
#' @param model_path   provide a full path to a model file.
#' @param feature_flag boolean. Should universal features be
#'                     included in the output.
#' @param parser       a character string of length 1, which is
#'                     either 'default' (default udpipe dependency
#'                     parsing) or 'none' (no dependency parsing needed)
#'                     or a character string with more complex parsing
#'                     options
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @examples
#'\dontrun{
#'cnlp_init_udpipe(model_name = "english")
#'}
#'
#' @export
cnlp_init_udpipe <- function(model_name = NULL,
                             model_path = NULL,
                             feature_flag = FALSE,
                             parser = "default") {
  if (!is.null(model_path))
    model_name <- "custom"
  if (is.null(model_name))
    model_name <- "english"
  if (length(feature_flag) != 1 | !is.logical(feature_flag))
    stop("feature_flag must be a length one boolean variable")
  if (length(parser) != 1 | !is.character(parser))
    stop("parser must be a length one character variable")

  volatiles$udpipe$model_name <- model_name
  volatiles$udpipe$model_path <- model_path
  volatiles$udpipe$feature_flag <- feature_flag
  volatiles$udpipe$parser <- parser

  invisible(init_udpipe_backend())
}


init_udpipe_backend <- function() {

  if (!requireNamespace("udpipe")) {
    stop("The udpipe package is required to use the udpipe backend.")
  }

  model_name <- volatiles$udpipe$model_name
  model_path <- volatiles$udpipe$model_path

  if (is.null(model_path)) {
    model_loc <- system.file("extdata", package="cleanNLP")
    model_path <- Sys.glob(file.path(model_loc,
                                    sprintf("%s-*.udpipe", model_name)))[1]

    # If model does not exist, download it here
    if (is.na(model_path)) {
      cnlp_download_udpipe(model_name = model_name, model_loc = model_loc)
    }

    model_path <- Sys.glob(file.path(model_loc,
                                    sprintf("%s-*.udpipe", model_name)))[1]
  } else {
    if (!file.exists(model_path)) {
      stop(sprintf("no file found at %s", model_path))
    }
  }

  volatiles$udpipe$model_path <- model_path
  volatiles$udpipe$model_obj <- udpipe::udpipe_load_model(model_path)

  volatiles$udpipe$init <- TRUE
  volatiles$model_init_last <- "udpipe"

  return(NULL)
}


annotate_with_udpipe <- function(input, as_strings) {

  if (!volatiles$udpipe$init) {
    stop("You must initialize udpipe with: init_udpipe_backend()")
  }

  # for now, we need to read all of the texts into R if they
  # are not already
  if (as_strings) {
    input_txt <- input
  } else {
    input_txt <- rep("", length(input))
    for (i in seq_along(input)) {
      input_txt[i] <- paste(readLines(input[i]), collapse = "\n")
    }
  }

  # call udpipe over the input
  anno <- udpipe::udpipe_annotate(volatiles$udpipe$model_obj,
                                  input_txt,
                                  parser = volatiles$udpipe$parser)

  # check the output
  if (anno$conllu == "") {
    stop(paste(anno$errors, collapse = "\n"))
  }

  # convert the CoNLL format into a cleanNLP object
  out <- from_udpipe_CoNLL(anno$conllu)

  # make a few minor adjustments to document table
  out$document$language <- basename(volatiles$udpipe$model_path)
  if (!as_strings) {
    out$document$uri <- input
  }

  return(out)
}

na_fill <- function (object)
{
  ok <- which(!is.na(object))
  if (is.na(object[1L])) {
    ok <- c(1L, ok)
  }

  gaps <- diff(c(ok, length(object) + 1L))
  return(rep(object[ok], gaps))
}


from_udpipe_CoNLL <- function(z) {

  # split into lines
  temp <- stringi::stri_split(z, fixed = "\n")[[1]]

  # find document ids
  index <- stringi::stri_sub(temp, 1, 11) == "# newdoc id"
  doc_id <- rep(NA_character_, length(temp))
  doc_id[index] <- stringi::stri_sub(temp[index], 15, -1)
  doc_id <- na_fill(doc_id)

  # find paragraphs
  index <- stringi::stri_sub(temp, 1, 8) == "# newpar"
  pid <- unlist(tapply(index, doc_id, cumsum), use.names = FALSE)

  # find sentence ids
  index <- stringi::stri_sub(temp, 1, 9) == "# sent_id"
  sid <- rep(NA_integer_, length(temp))
  sid[index] <- as.integer(stringi::stri_sub(temp[index], 13, -1))
  sid <- na_fill(sid)

  # remove comments and empty lines
  ok <- !(stringi::stri_sub(temp, 1, 1) %in% c("", "#"))
  temp <- temp[ok]
  doc_id <- doc_id[ok]
  sid <- sid[ok]
  pid <- pid[ok]

  # parse the body of the CoNLL
  body <- utils::read.delim(
                     text = temp, sep = "\t", na.strings = "_",
                     quote = "",
                     header = FALSE, stringsAsFactors = FALSE,
                     colClasses = c("character", "character", "character",
                                    "character", "character", "character",
                                    "integer", "character", "character",
                                    "character"))

  # for now, remove multi-token terms
  suppressWarnings({ tid <- as.integer(body$V1) })
  ok <- !is.na(tid)
  doc_id <- doc_id[ok]
  sid <- sid[ok]
  pid <- pid[ok]
  tid <- tid[ok]
  body <- body[ok,]

  # extract features from the CoNLL body
  word <- body$V2
  lemma <- body$V3
  upos <- body$V4
  pos <- body$V5
  relation <- body$V8
  source <- body$V7
  feats <- body$V6

  # detect character offsets
  s_after <- stringi::stri_match(body$V10, regex = "SpacesAfter=([^|]+)")[,2]
  s_after[is.na(s_after)] <- " "
  s_after[stringi::stri_detect(body$V10, fixed = "SpaceAfter=No")] <- ""
  s_after <- stringi::stri_replace_all(s_after, "\t", fixed = "\\t")
  s_after <- stringi::stri_replace_all(s_after, "\n", fixed = "\\n")
  s_after <- stringi::stri_replace_all(s_after, " ", fixed = "\\s")
  cid <- stringi::stri_length(s_after)
  cid <- cid + stringi::stri_length(word)
  cid <- tapply(cid, doc_id, function(v) cumsum(c(0, v[-length(v)])))
  cid <- unlist(cid, use.names = FALSE)
  cid <- as.integer(cid)

  # create document table
  doc <- data.frame(id       = unique(doc_id),
                    time     = format(Sys.time(), fmt = "%dZ", tz = "UTC"),
                    version  = as.character(utils::packageVersion("udpipe")),
                    language = NA_character_,
                    uri     = NA_character_,
                    stringsAsFactors = FALSE)

  # create token table
  token <- data.frame(id = doc_id, sid = sid, tid = tid,
                word = word, lemma = lemma,
                upos = upos, pos = pos,
                cid = cid, pid = pid,
                s_after = s_after,
                stringsAsFactors = FALSE)

  # add extra features to token table
  if (volatiles$udpipe$feature_flag) {
    temp <- stringi::stri_split(feats, fixed = "|")

    index <- mapply(function(u, v) rep(u, length(v)), seq_along(temp), temp)
    df <- data.frame(index = unlist(index),
                     raw = unlist(temp))
    temp <- stringi::stri_match(df$raw, regex = "([A-Za-z]+)=([A-Za-z0-9,]+)")
    df$key <- temp[,2]
    df$val <- temp[,3]
    df <- df[!is.na(df$raw),]
    df$key <- stringi::stri_replace_all(df$key, "$1_$2", regex = "([a-z])([A-Z])")
    df$key <- tolower(df$key)

    vars <- sort(unique(df$key))
    for (v in vars) {
      df_var <- df[df$key == v,]
      token[[v]] <- NA_character_
      token[[v]][df_var$index] <- df_var$val
    }
  }

  # add roots to the token table
  roots <- token[tid == 1,]
  roots$tid <- 0L
  roots$word <- roots$lemma <- "ROOT"
  roots$upos <- roots$pos <- NA_character_
  token <- rbind(token, roots)
  token_id_num <- as.numeric(stringi::stri_sub(token$id, 4, -1))
  token <- token[order(token_id_num, token$sid, token$tid),]

  # create dependency table
  tid_target <- tid
  dep <- data.frame(id = doc_id,
                    sid = sid,
                    tid = source,
                    tid_target = tid_target,
                    relation = relation,
                    relation_full = relation,
                    stringsAsFactors = FALSE)
  dep <- dep[!is.na(dep$relation),]

  # create annotation object
  anno <- empty_anno()
  anno$dependency <- structure(dep,
                          class = c("tbl_df", "tbl", "data.frame"))
  anno$document <- structure(doc,
                          class = c("tbl_df", "tbl", "data.frame"))
  anno$token <- structure(token,
                          class = c("tbl_df", "tbl", "data.frame"))


  return(anno)
}