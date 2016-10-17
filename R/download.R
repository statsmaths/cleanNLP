#' Download java files needed for cleanNLP
#'
#' The cleanNLP package does not supply the raw java files
#' provided by the Stanford NLP Group as they are quite large.
#' This function downloads the libraries automatically, by default into
#' the directory where the package was installed.
#'
#' In order to manually download files, simply unzip them and
#' specify their location in your call to \code{\link{init_clean_nlp}}
#' using the \code{lib_location} parameter.
#'
#' @importFrom           utils download.file unzip
#' @param output_loc     a string showing where the files are to be downloaded.
#'                       If missing, will try to download files into the directory
#'                       where the package was original installed.
#' @param type           type of files to download. The base backage, installed by
#'                       default, is required. Other jars include chinese, german,
#'                       and spanish. These can be installed in addition to the
#'                       base package.
#'@examples
#'\dontrun{
#'download_clean_nlp()
#'download_clean_nlp(type="spanish")
#'}
#' @export
download_clean_nlp = function(output_loc,
    type=c("base","chinese","english", "french", "german", "spanish")) {

  baseURL <- "http://nlp.stanford.edu/software/"
  coreFile <- "/stanford-corenlp-full-2015-12-09"

  type = match.arg(type)
  if (missing(output_loc)) {
    output_loc = system.file("extdata", package="cleanNLP")
    if (file.access(output_loc, "6") == -1)
      stop("You do not have read+write access to location where the",
           "cleanNLP package is installed! You must specify an output",
           "location with output_loc.")
  }

  if (type == "base") {
    ret <- download.file(paste0(baseURL,coreFile,".zip"), destfile = file.path(output_loc, paste0(coreFile, ".zip")))
    if (ret != 0) stop("Download error!")

    unzip(file.path(output_loc, paste0(coreFile, ".zip")), exdir = output_loc)
    file.remove(file.path(output_loc, paste0(coreFile, ".zip")))
    return(0L)
  }

  if (!file.exists(file.path(output_loc)))
    stop("Must download base files to this location first! Set type='base'.")

  if (type == "chinese")
    download.file(paste0(baseURL, "/stanford-chinese-corenlp-2016-01-19-models.jar"),
      destfile = file.path(output_loc, coreFile, "/stanford-chinese-corenlp-2016-01-19-models.jar"))

  if (type == "english")
    download.file(paste0(baseURL, "/stanford-english-corenlp-2016-01-10-models.jar"),
      destfile=file.path(output_loc, coreFile, "/stanford-english-corenlp-2016-01-10-models.jar"))

  if (type == "french")
    download.file(paste0(baseURL, "/stanford-french-corenlp-2016-01-14-models.jar"),
      destfile=file.path(output_loc, coreFile, "/stanford-french-corenlp-2016-01-14-models.jar"))

  if (type == "german")
    download.file(paste0(baseURL, "/stanford-german-corenlp-2016-01-19-models.jar"),
      destfile=file.path(output_loc, coreFile, "/stanford-german-corenlp-2016-01-19-models.jar"))

  if (type == "spanish")
    download.file(paste0(baseURL, "/stanford-spanish-corenlp-2015-10-14-models.jar"),
      destfile=file.path(output_loc, coreFile, "/stanford-spanish-corenlp-2015-10-14-models.jar"))

}
