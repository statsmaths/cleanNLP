#' Download java files needed for cleanNLP
#'
#' The cleanNLP package does not supply the raw java files
#' provided by the Stanford NLP Group as they are quite large.
#' This function downloads the libraries for you, by default into
#' the directory where the package was installed.
#'
#' If you want to manually download files, simply unzip them and
#' place in \code{system.file("extdata", package="cleanNLP")}
#'
#' @importFrom        utils download.file unzip
#' @param outputLoc      a string showing where the files are to be downloaded.
#'                       If missing, will try to download files into the directory
#'                       where the package was original installed.
#' @param type           type of files to download. The base backage, installed by
#'                       default is required. Other jars include chinese, german,
#'                       and spanish. These will be installed in addition to the
#'                       base package.
#'@examples
#'\dontrun{
#'downloadCleanNLP()
#'downloadCleanNLP(type="spanish")
#'}
#' @export
downloadCleanNLP = function(outputLoc,
    type=c("base","chinese","english", "french", "german", "spanish")) {

  baseURL = "http://nlp.stanford.edu/software/"
  coreFile = "/stanford-corenlp-full-2015-12-09"

  type = match.arg(type)
  if (missing(outputLoc)) {
    outputLoc = system.file("extdata",package="coreNLP")
    if (file.access(outputLoc,"6") == -1)
      stop("You do not have read+write access to location where the",
           "coreNLP package is installed! You must specify an output",
           "location with outputLoc.")
  }

  if (type == "base") {
    ret = download.file(paste0(baseURL,coreFile,".zip"), destfile=paste0(outputLoc, coreFile, ".zip"))
    if (ret != 0) stop("Download error!")

    unzip(paste0(outputLoc, coreFile, ".zip"), exdir=outputLoc)
    file.remove(paste0(outputLoc, coreFile, ".zip"))
    return(0L)
  }

  if (!file.exists(paste0(outputLoc, "/", coreFile)))
    stop("Must download base files to this location first! Set type='base'.")

  if (type == "chinese")
    download.file(paste0(baseURL, "/stanford-chinese-corenlp-2016-01-19-models.jar"),
      destfile=paste0(outputLoc, coreFile, "/stanford-chinese-corenlp-2016-01-19-models.jar"))

  if (type == "english")
    download.file(paste0(baseURL, "/stanford-english-corenlp-2016-01-10-models.jar"),
      destfile=paste0(outputLoc, coreFile, "/stanford-english-corenlp-2016-01-10-models.jar"))

  if (type == "french")
    download.file(paste0(baseURL, "/stanford-french-corenlp-2016-01-14-models.jar"),
      destfile=paste0(outputLoc, coreFile, "/stanford-french-corenlp-2016-01-14-models.jar"))

  if (type == "german")
    download.file(paste0(baseURL, "/stanford-german-corenlp-2016-01-19-models.jar"),
      destfile=paste0(outputLoc, coreFile, "/stanford-german-corenlp-2016-01-19-models.jar"))

  if (type == "spanish")
    download.file(paste0(baseURL, "/stanford-spanish-corenlp-2015-10-14-models.jar"),
      destfile=paste0(outputLoc, coreFile, "/stanford-spanish-corenlp-2015-10-14-models.jar"))

}
