#' Interface for initializing the udpipe backend
#'
#' This function must be run before annotating text with
#' the tokenizers backend. It sets the properties for the
#' spaCy engine and loads the file using the R to Python
#' interface provided by reticulate.
#'
#' @param model_name   string giving the model namel.
#'                     Defaults to "english" if NULL.
#'                     Ignored if \code{model_path} is not NULL.
#' @param model_path   provide a full path to a model file.
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
                        model_path = NULL) {
  if (!is.null(model_path))
    model_name <- "custom"
  if (is.null(model_name))
    model_name <- "english"

  volatiles$udpipe$model_name <- model_name
  volatiles$udpipe$model_path <- model_path

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
      udpipe::udpipe_download_model(language = model_name,
                                    model_dir = model_loc)
      model_path <- Sys.glob(file.path(model_loc,
                                       sprintf("%s-*.udpipe", model_name)))[1]
    }
  }

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
                                  input_txt)

  # convert the CoNLL format into a cleanNLP object
  out <- from_udpipe_CoNLL(anno$conllu)

  # make a few minor adjustments to document table
  out$document$language <- volatiles$udpipe$model_name
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

  # parse the body of the CoNLL
  body <- utils::read.delim(
                     text = temp, sep = "\t", na.strings = "_",
                     header = FALSE, stringsAsFactors = FALSE,
                     colClasses = c("integer", "character", "character",
                                    "character", "character", "character",
                                    "integer", "character", "character",
                                    "character"))

  tid <- body$V1
  word <- body$V2
  lemma <- body$V3
  upos <- body$V4
  pos <- body$V5
  relation <- body$V8
  source <- body$V7

  # detect character offsets
  cid <- stringi::stri_match(body$V10, regex = "SpacesAfter=([^|]+)")[,2]
  cid <- stringi::stri_length(cid)
  cid[stringi::stri_detect(body$V10, fixed = "SpaceAfter=No")] <- 0
  cid[is.na(cid)] <- 1
  cid <- cid + stringi::stri_length(word)
  cid <- tapply(cid, doc_id, function(v) cumsum(c(0, v[-length(v)])))
  cid <- unlist(cid, use.names = FALSE)

  # create document table
  doc <- data.frame(doc_id   = unique(doc_id),
                    time     = format(Sys.time(), fmt = "%dZ", tz = "UTC"),
                    version  = as.character(utils::packageVersion("udpipe")),
                    language = NA_character_,
                    uri     = NA_character_,
                    stringsAsFactors = FALSE)

  # create token table
  token <- data.frame(doc_id = doc_id, sid = sid, tid = tid,
                word = word, lemma = lemma,
                upos = upos, pos = pos,
                cid = cid,
                stringsAsFactors = FALSE)

  roots <- token[tid == 1,]
  roots$tid <- 0L
  roots$word <- roots$lemma <- "ROOT"
  roots$upos <- roots$pos <- NA_character_
  token <- rbind(token, roots)
  token <- token[order(token$doc_id, token$sid, token$tid),]

  # create dependency table
  tid_target <- tid
  dep <- data.frame(doc_id = doc_id,
                    sid = sid,
                    tid = source,
                    tid_target = tid_target,
                    relation = relation,
                    relation_full = relation,
                    stringsAsFactors = FALSE)


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