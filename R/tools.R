#' Compute Principal Components and store as a Data Frame
#'
#' Takes a matrix, perhaps from the output of \code{\link{cnlp_utils_tfidf}},
#' and returns a data frame with the top principal components extracted. This
#' is a simple but powerful technique for visualizing a corpus of documents.
#'
#' @param x       a matrix object to pass to \code{prcomp}
#' @param meta    an optional object to append to the front of the
#'                principal components. Can be a vector or a data frame,
#'                but must have the same length or number of rows as the
#'                number of rows in \code{x}
#' @param k       integer. The number of components to include in the output.
#' @param center  logical. Should the data be centered?
#' @param scale   logical. Should the data be scaled? Note that this will
#'                need to be set to false if any columns in \code{x} are
#'                constant if \code{center} is also true.
#'
#' @return a \code{data_frame} object containing the top \code{k} principal
#'         components of the data in x, with the object \code{meta} appended
#'         to the front, when it is non-null.
#' @examples
#' require(dplyr)
#' data(obama)
#'
#' # Get principal components from the non-proper noun lemmas
#' tfidf <- cnlp_get_token(obama) %>%
#'   filter(pos %in% c("NN", "NNS")) %>%
#'   cnlp_utils_tfidf()
#' pca_doc <- cnlp_utils_pca(tfidf, cnlp_get_document(obama))
#'
#' # Plot speeches using the first two principal components
#' plot(pca_doc$PC1, pca_doc$PC2, col = "white")
#' text(pca_doc$PC1, pca_doc$PC2, label = 2009:2016)
#'
#' @export
cnlp_utils_pca <- function(x, meta = NULL, k = 2, center = TRUE,
                           scale = TRUE) {

  m <- stats::prcomp(x, center = center, scale. = scale)$x
  out <- dplyr::as_data_frame(m[,1:k])

  if (!is.null(meta) && !is.data.frame(meta))
    meta <- dplyr::as_data_frame(meta)

  dplyr::bind_cols(meta, out)
}

#' Construct the TF-IDF Matrix from Annotation or Data Frame
#'
#' Given an annotation object, this function returns the term-frequency
#' inverse document frequency (tf-idf) matrix from the extracted lemmas.
#' A data frame with a document id column and token column can be also
#' be given, which allows the user to preprocess and filter the desired
#' tokens to include.
#'
#' @param  object       either an annotation object or a data frame with
#'                      columns equal to the inputs given to
#'                      \code{doc_var} and \code{token_var}
#' @param  type         the desired return type. The options \code{tfidf},
#'                      \code{tf}, and \code{idf} return a list with
#'                      the desired matrix, the document ids, and the
#'                      vocabulary set. The option \code{all} returns
#'                      a list with all three as well as the ids and
#'                      vocabulary. For consistency, \code{vocab} all
#'                      returns a list but this only contains the ids
#'                      and vocabulary set.
#' @param  tf_weight    the weighting scheme for the term frequency matrix.
#'                      The selection \code{lognorm} takes one plus
#'                      the log of the raw frequency (or zero if zero),
#'                      \code{binary} encodes a zero one matrix
#'                      indicating simply whether the token exists at all
#'                      in the document, \code{raw} returns raw counts,
#'                      and \code{dnorm} uses double normalization.
#' @param idf_weight    the weighting scheme for the inverse document
#'                      matrix. The selection \code{idf} gives the
#'                      logarithm of the simple inverse frequency,
#'                      \code{smooth} gives the logarithm of one plus
#'                      the simple inverse frequency, and \code{prob}
#'                      gives the log odds of the the token occurring
#'                      in a randomly selected document.
#' @param min_df        the minimum proportion of documents a token
#'                      should be in to be included in the vocabulary
#' @param max_df        the maximum proportion of documents a token
#'                      should be in to be included in the vocabulary
#' @param max_features  the maximum number of tokens in the vocabulary
#' @param doc_var       character vector. The name of the column in
#'                      \code{object} that contains the document ids,
#'                      unless \code{object} is an annotation object,
#'                      in which case it's the column of the token
#'                      matrix to use as the document id.
#' @param token_var     character vector. The name of the column in
#'                      \code{object} that contains the tokens,
#'                      unless \code{object} is an annotation object,
#'                      in which case it's the column of the token
#'                      matrix to use as the tokens (generally either
#'                      \code{lemma} or \code{word}).
#' @param vocabulary    character vector. The vocabulary set to use in
#'                      constructing the matrices. Will be computed
#'                      within the function if set to \code{NULL}. When
#'                      supplied, the options \code{min_df}, \code{max_df},
#'                      and \code{max_features} are ignored.
#' @param doc_set       optional character vector of document ids. Useful to
#'                      create empty rows in the output matrix for documents
#'                      without data in the input. Most users will want to keep
#'                      this equal to \code{NULL}, the default, to have the 
#'                      function compute the document set automatically.
#' @param ...           other arguments passed to the base method
#'
#' @return  a sparse matrix with dimnames or, if "all", a list with elements
#'\itemize{
#' \item{tf}{ the term frequency matrix}
#' \item{idf}{ the inverse document frequency matrix}
#' \item{tfidf}{ the product of the tf and idf matrices}
#' \item{vocab}{ a character vector giving the vocabulary used in
#'               the function, corresponding to the columns of the
#'               matrices}
#' \item{id}{ a vector of the doc ids, corresponding to the rows of
#'                the matrices}
#'}
#'
#'
#' @examples
#' require(dplyr)
#' data(obama)
#'
#' # Top words in the first Obama S.O.T.U., using all tokens
#' tfidf <- cnlp_utils_tfidf(obama)
#' vids <- order(tfidf[1,], decreasing = TRUE)[1:10]
#' colnames(tfidf)[vids]
#'
#' # Top words, only using non-proper nouns
#' tfidf <- cnlp_get_token(obama) %>%
#'   filter(pos %in% c("NN", "NNS")) %>%
#'   cnlp_utils_tfidf()
#' vids <- order(tfidf[1,], decreasing = TRUE)[1:10]
#' colnames(tfidf)[vids]
#'
#' @export
#' @name cnlp_utils_tfidf
cnlp_utils_tfidf <- function(object,
                      type = c("tfidf", "tf", "idf", "vocab", "all"),
                      tf_weight = c("lognorm", "binary", "raw", "dnorm"),
                      idf_weight = c("idf", "smooth", "prob"),
                      min_df = 0.1,
                      max_df = 0.9,
                      max_features = 1e4,
                      doc_var = c("doc_id", "id"),
                      token_var = "lemma",
                      vocabulary = NULL,
                      doc_set = NULL) {

  if (inherits(object, "annotation"))
    object <- cnlp_get_token(object)

  count <- prop <- token <- NULL # silence r check

  doc_var <- doc_var[min(which(doc_var %in% names(object)))]
  if (length(doc_var) == 0) stop("No valid doc_var found; please specify")

  type <- match.arg(type)
  tf_weight <- match.arg(tf_weight)
  idf_weight <- match.arg(idf_weight)
  x <- dplyr::data_frame(doc = object[[doc_var]],
                         token = object[[token_var]])

  if (is.null(vocabulary)) {

    N <- length(unique(x$doc))

    possible_vocab <- unique(x)
    possible_vocab <- dplyr::group_by_(possible_vocab, "token")
    possible_vocab <- dplyr::summarize_(possible_vocab, prop = "n()")
    possible_vocab$prop <- possible_vocab$prop / N
    possible_vocab <- dplyr::filter_(possible_vocab, ~ prop > min_df,
                                     ~ prop < max_df)
    possible_vocab <- possible_vocab$token

    vocabulary <- dplyr::filter_(x, ~ token %in% possible_vocab)
    vocabulary <- dplyr::group_by_(vocabulary, "token")
    vocabulary <- dplyr::summarize_(vocabulary, n = "n()")
    vocabulary <- dplyr::arrange_(vocabulary, "dplyr::desc(n)")

    index <- 1:min(c(max_features, nrow(vocabulary)))
    vocabulary <- vocabulary[["token"]][index]

  }

  if (length(vocabulary) <= 2) {
    stop("vocabulary length is too small to continue")
  }

  # create counts
  if (is.null(doc_set)) {
    doc_set <- unique(x[["doc"]])
  }
  x <- dplyr::filter_(x, ~ token %in% vocabulary)
  x$token <- factor(x$token, levels = vocabulary)
  doc <- x[["doc"]]
  N <- length(doc_set)
  id <- match(doc, doc_set)
  mat <- methods::as(Matrix::sparse.model.matrix(~ token - 1, data = x),
                     "dgTMatrix")

  df <- dplyr::data_frame(id = id[mat@i + 1], lid = mat@j, count = mat@x)
  df <- dplyr::group_by_(df, "id", "lid")
  df <- dplyr::summarize_(df, count = "sum(count)")

  term_counts <- Matrix::spMatrix(nrow = length(doc_set), ncol = ncol(mat),
                                  i = df$id, j = df$lid + 1, x = df$count)

  # tf
  if (type %in% c("tfidf", "tf", "idf", "all")) {

    if (tf_weight == "lognorm") {
      tf <- term_counts
      tf@x <- 1 + log2(tf@x)
      tf@x[tf@x < 0] <- 0
    } else if (tf_weight == "binary") {
      tf <- term_counts * 1.0
      tf[term_counts != 0] <- 1L
    } else if (tf_weight == "raw") {
      tf <- term_counts
    } else if (tf_weight == "dnorm") {
      tf <- 0.5 + 0.5 * term_counts / apply(term_counts, 1, max)
    }

  }

  # idf
  if (type %in% c("tfidf", "idf", "all")) {

    if (idf_weight == "idf") {

      idf <- log2(N / apply(term_counts > 0, 2, sum))

    } else if (idf_weight == "smooth") {

      idf <- log2(1 + N / apply(term_counts > 0, 2, sum))

    } else if (idf_weight == "prob") {

      n <- apply(term_counts > 0, 2, sum)
      idf <- log2( (N - n) / n)

    }

  }

  # tf-idf
  if (type %in% c("tfidf", "all")) {
    tfidf <- Matrix::t(Matrix::t(tf) * idf)
  }

  # Select output:
  if (type == "tfidf") {

    out <- tfidf
    rownames(out) <- doc_set
    colnames(out) <- vocabulary

  } else if (type == "tf") {

    out <- tf
    rownames(out) <- doc_set
    colnames(out) <- vocabulary

  } else if (type == "idf") {

    out <- idf
    names(out) <- vocabulary

  } else if (type == "vocab") {

    out <- vocabulary

  } else if (type == "all") {

    rownames(tf) <- doc_set
    colnames(tf) <- vocabulary
    names(idf) <- vocabulary
    rownames(tfidf) <- doc_set
    colnames(tfidf) <- vocabulary

    out <- list(tf = tf, idf = idf, tfidf = tfidf, id = doc_set,
                vocab = vocabulary)

  } else {

    stop(sprintf("Something is wrong with the type argument ('%s')", type))

  }

  return(out)

}

#' @rdname cnlp_utils_tfidf
#' @export
cnlp_utils_tf <- function(object, type = "tf", tf_weight = "raw", ...) {
  out <- cnlp_utils_tfidf(object, type = type, tf_weight = tf_weight, ...)
  return(out)
}


