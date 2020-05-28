#' Download java files needed for CoreNLP
#'
#' The cleanNLP package does not supply the raw java files
#' provided by the Stanford NLP Group as they are quite large.
#' This function downloads the libraries automatically, by default
#' into the directory where the package was installed. These are
#' not required for using the spaCy Python implementation.
#'
#' @param type           type of files to download. The base package
#                        is always required.
#'                       Other jars include model files for French, German,
#'                       and Spanish. These can be installed in addition to
#'                       the base package. By default, the function downloads
#'                       the base package and English model files.
#' @param output_loc     a string showing where the files are to be
#'                       downloaded. If missing, will try to download
#'                       files into the directory where the package was
#'                       original installed.
#' @param url            the url to try to download components from. Setting
#'                       to NULL uses the default location on the Stanford
#'                       NLP server, but you can set this manually by using
#'                       this option. It also allows for local files, but
#'                       note that the path must include the prefix
#'                       \code{file://}. For details, see
#'                       \code{\link{download.file}}.
#' @param url_core       if \code{url} is not null, this flag indicates
#'                       whether the path given to url points to the core
#'                       nlp files (which are zipped) or
#'                       to model files (which are unzipped).
#' @param force          logical. Should files be downloaded if they appear
#'                       to already exist?
#'
#'@examples
#'\dontrun{
#'cnlp_download_corenlp()
#'cnlp_download_corenlp(type="spanish")
#'}
#' @export
cnlp_download_corenlp <- function(
    type = c("default", "base", "en", "fr", "de", "es", "ar", "zh"),
    output_loc, url = NULL, url_core = TRUE, force = FALSE) {

  if (!requireNamespace("RCurl"))
    stop("You must install RCurl to download the coreNLP files.")

  # set defaults and determine where files should be saved
  baseURL <- "https://nlp.stanford.edu/software/"
  coreFile <- "/stanford-corenlp-full-2018-10-05"
  if (missing(output_loc)) {
    output_loc <- system.file("extdata", package="cleanNLP")
    if (file.access(output_loc, "6") == -1)
      stop("You do not have read+write access to location where the",
           "cleanNLP package is installed! You must specify an output",
           "location with output_loc.")
  }

  if (!dir.exists(output_loc)) {
    stop(sprintf("The output directory '%s' does not exist",
      output_loc))
  }

  # if url is given, simply download the specified files as required
  if (!is.null(url)) {
    if (url_core) {
      if (!dir.exists(file.path(output_loc, coreFile))) {
        fp <- check_file_exists(file.path(output_loc,
                                           paste0(coreFile, ".zip")),
                                 force = force)
        f <- RCurl::CFILE(fp, mode="wb")
        ret <- RCurl::curlPerform(url = url, writedata = f@ref,
                                  noprogress=FALSE)
        RCurl::close(f)
        if (ret != 0) stop("Download error!")

        utils::unzip(file.path(output_loc, paste0(coreFile, ".zip")),
                     exdir = output_loc)
        file.remove(file.path(output_loc, paste0(coreFile, ".zip")))
        return(0L)
      }
    } else {
      fname <- basename(url)
      fp <- check_file_exists(file.path(output_loc, coreFile, fname),
                               force = force)
      f <- RCurl::CFILE(fp, mode="wb")
      ret <- RCurl::curlPerform(url = url, writedata = f@ref,
                                noprogress=FALSE)
      RCurl::close(f)
    }
  }

  # otherwise, determine what file types should be downloaded
  type <- match.arg(type)

  if (type %in% c("default", "base")) {
    if (!dir.exists(file.path(output_loc, coreFile))) {
      fp <- check_file_exists(file.path(output_loc, paste0(coreFile, ".zip")),
                               force = force)
      f <- RCurl::CFILE(fp, mode="wb")
      ret <- RCurl::curlPerform(url = paste0(baseURL, coreFile, ".zip"),
                                writedata = f@ref, noprogress=FALSE)
      RCurl::close(f)
      if (ret != 0) stop("Download error!")

      utils::unzip(file.path(output_loc, paste0(coreFile, ".zip")),
                   exdir = output_loc)
      file.remove(file.path(output_loc, paste0(coreFile, ".zip")))
      ret <- 0L
    }
  }

  if (!file.exists(file.path(output_loc)))
    stop("Must download base files to this location first!",
         "Set type='base'.")

  if (type %in% c("default", "en")) {
    fp <- check_file_exists(file.path(output_loc, coreFile,
             "/stanford-english-corenlp-2018-10-05-models.jar"),
             force = force)
    f <- RCurl::CFILE(fp, mode="wb")
    ret <- RCurl::curlPerform(url = paste0(baseURL,
                  "/stanford-english-corenlp-2018-10-05-models.jar"),
                  writedata = f@ref, noprogress=FALSE)
    RCurl::close(f)
  }

  if (type %in% c("fr")) {
    fp <- check_file_exists(file.path(output_loc, coreFile,
             "/stanford-french-corenlp-2018-10-05-models.jar"),
             force = force)
    f <- RCurl::CFILE(fp, mode="wb")
    ret <- RCurl::curlPerform(url = paste0(baseURL,
             "/stanford-french-corenlp-2018-10-05-models.jar"),
             writedata = f@ref, noprogress=FALSE)
    RCurl::close(f)
  }

  if (type %in% c("de")) {
    fp <- check_file_exists(file.path(output_loc, coreFile,
             "/stanford-german-corenlp-2018-10-05-models.jar"),
             force = force)
    f <- RCurl::CFILE(fp, mode="wb")
    ret <- RCurl::curlPerform(url = paste0(baseURL,
             "/stanford-german-corenlp-2018-10-05-models.jar"),
             writedata = f@ref, noprogress=FALSE)
    RCurl::close(f)
  }

  if (type %in% c("es")) {
    fp <- check_file_exists(file.path(output_loc, coreFile,
             "/stanford-spanish-corenlp-2018-10-05-models.jar"),
             force = force)
    f <- RCurl::CFILE(fp, mode="wb")
    ret <- RCurl::curlPerform(url = paste0(baseURL,
             "/stanford-spanish-corenlp-2018-10-05-models.jar"),
             writedata = f@ref, noprogress=FALSE)
    RCurl::close(f)
  }

  if (type %in% c("ar")) {
    fp <- check_file_exists(file.path(output_loc, coreFile,
             "/stanford-arabic-corenlp-2018-10-05-models.jar"),
             force = force)
    f <- RCurl::CFILE(fp, mode="wb")
    ret <- RCurl::curlPerform(url = paste0(baseURL,
             "/stanford-arabic-corenlp-2018-10-05-models.jar"),
             writedata = f@ref, noprogress=FALSE)
    RCurl::close(f)
  }

  if (type %in% c("zh")) {
    fp <- check_file_exists(file.path(output_loc, coreFile,
             "/stanford-chinese-corenlp-2018-10-05-models.jar"),
             force = force)
    f <- RCurl::CFILE(fp, mode="wb")
    ret <- RCurl::curlPerform(url = paste0(baseURL,
             "/stanford-chinese-corenlp-2018-10-05-models.jar"),
             writedata = f@ref, noprogress=FALSE)
    RCurl::close(f)
  }

  ret
}

check_file_exists <- function(path, force = FALSE) {
  if (file.exists(path) & !force) {
    stop(sprintf("file already exists at: %s", path))
  } else if (file.exists(path) & force) {
    message(sprintf("overwriting file at: %s", path))
  }

  return(path)
}

#' Download model files needed for udpipe
#'
#' The cleanNLP package does not supply the model files required
#' for using the udpipe backend. These files can be downloaded
#' with this function. The models are saved, by default, in the
#' location where the package is installed. They will be saved
#' between R sessions. This function is called internally by
#' \code{\link{cnlp_init_udpipe}} if a model is not available.
#'
#' @param model_name   string giving the model namel.
#'                     Defaults to "english" if NULL.
#' @param model_loc    where should be model be downloaded to. If
#'                     set to NULL, will be downloaded in the location
#'                     that the cleanNLP package is installed.
#'
#'@examples
#'\dontrun{
#'cnlp_download_core_nlp()
#'cnlp_download_core_nlp(type="spanish")
#'}
#' @export
cnlp_download_udpipe <- function(model_name = "english", model_loc = NULL) {
  if (is.null(model_loc)) {
    model_loc <- system.file("extdata", package="cleanNLP")
  }

  udpipe::udpipe_download_model(language = model_name,
                                model_dir = model_loc)

}