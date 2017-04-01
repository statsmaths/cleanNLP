#' Download java files needed for CoreNLP
#'
#' The cleanNLP package does not supply the raw java files
#' provided by the Stanford NLP Group as they are quite large.
#' This function downloads the libraries automatically, by default into
#' the directory where the package was installed. These are not
#' required for using the spaCy Python implementation.
#'
#' @param type           type of files to download. The base backage is always required.
#'                       Other jars include model files for French, German,
#'                       and Spanish. These can be installed in addition to the
#'                       base package. By default, the function downloads the base
#'                       package and English model files.
#' @param output_loc     a string showing where the files are to be downloaded.
#'                       If missing, will try to download files into the directory
#'                       where the package was original installed.
#' @param url            the url to try to download components from. Setting to NULL
#'                       uses the default location on the Stanford NLP server, but
#'                       you can set this manually by using this option. It also allows
#'                       for local files, but note that the path must include the prefix
#'                       \code{file://}. For details, see \code{\link{download.file}}.
#' @param url_core       if \code{url} is not null, this flag indicates whehter the path
#'                       given to url points to the core nlp files (which are zipped) or
#'                       to model files (which are unzipped).
#'
#'@examples
#'\dontrun{
#'download_core_nlp()
#'download_core_nlp(type="spanish")
#'}
#' @export
download_core_nlp = function(type = c("default", "base", "en", "fr", "de", "es"),
    output_loc, url = NULL, url_core = TRUE) {

  if (!requireNamespace("RCurl"))
    stop("You must install RCurl to download the coreNLP files.")

  # set defaults and determine where files should be saved
  baseURL <- "https://nlp.stanford.edu/software/"
  coreFile <- "/stanford-corenlp-full-2016-10-31"
  if (missing(output_loc)) {
    output_loc <- system.file("extdata", package="cleanNLP")
    if (file.access(output_loc, "6") == -1)
      stop("You do not have read+write access to location where the",
           "cleanNLP package is installed! You must specify an output",
           "location with output_loc.")
  }

  # if url is given, simply download the specified files as required
  if (!is.null(url)) {
    if (url_core) {
      f <- RCurl::CFILE(file.path(output_loc, paste0(coreFile, ".zip")), mode="wb")
      ret <- RCurl::curlPerform(url = url, writedata = f@ref, noprogress=FALSE)
      RCurl::close(f)
      if (ret != 0) stop("Download error!")

      utils::unzip(file.path(output_loc, paste0(coreFile, ".zip")), exdir = output_loc)
      file.remove(file.path(output_loc, paste0(coreFile, ".zip")))
      return(0L)
    } else {
      fname <- basename(url)
      f <- RCurl::CFILE(file.path(output_loc, coreFile, fname), mode="wb")
      ret <- RCurl::curlPerform(url = url, writedata = f@ref, noprogress=FALSE)
      RCurl::close(f)
    }
  }

  # otherwise, determine what file types should be downloaded
  type <- match.arg(type)

  if (type %in% c("default", "base")) {
    f <- RCurl::CFILE(file.path(output_loc, paste0(coreFile, ".zip")), mode="wb")
    ret <- RCurl::curlPerform(url = paste0(baseURL, coreFile, ".zip"), writedata = f@ref, noprogress=FALSE)
    RCurl::close(f)
    if (ret != 0) stop("Download error!")

    utils::unzip(file.path(output_loc, paste0(coreFile, ".zip")), exdir = output_loc)
    file.remove(file.path(output_loc, paste0(coreFile, ".zip")))
    ret <- 0L
  }

  if (!file.exists(file.path(output_loc)))
    stop("Must download base files to this location first! Set type='base'.")

  if (type %in% c("default", "en")) {
    f <- RCurl::CFILE(file.path(output_loc, coreFile, "/stanford-english-corenlp-2016-10-31-models.jar"), mode="wb")
    ret <- RCurl::curlPerform(url = paste0(baseURL, "/stanford-english-corenlp-2016-10-31-models.jar"), writedata = f@ref, noprogress=FALSE)
    RCurl::close(f)
  }

  if (type %in% c("fr")) {
    f <- RCurl::CFILE(file.path(output_loc, coreFile, "/stanford-french-corenlp-2016-10-31-models.jar"), mode="wb")
    ret <- RCurl::curlPerform(url = paste0(baseURL, "/stanford-french-corenlp-2016-10-31-models.jar"), writedata = f@ref, noprogress=FALSE)
    RCurl::close(f)
  }

  if (type %in% c("de")) {
    f <- RCurl::CFILE(file.path(output_loc, coreFile, "/stanford-german-corenlp-2016-10-31-models.jar"), mode="wb")
    ret <- RCurl::curlPerform(url = paste0(baseURL, "/stanford-german-corenlp-2016-10-31-models.jar"), writedata = f@ref, noprogress=FALSE)
    RCurl::close(f)
  }

  if (type %in% c("es")) {
    f <- RCurl::CFILE(file.path(output_loc, coreFile, "/stanford-spanish-corenlp-2016-10-31-models.jar"), mode="wb")
    ret <- RCurl::curlPerform(url = paste0(baseURL, "/stanford-spanish-corenlp-2016-10-31-models.jar"), writedata = f@ref, noprogress=FALSE)
    RCurl::close(f)
  }

  ret
}
