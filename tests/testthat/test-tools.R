library(cleanNLP)

context("Testing tools for working with textual data")

data(obama)

test_that("testing get_tfidf", {
  tf_direct <- get_tfidf(obama, type = "all")
  vlen <- length(tf_direct$vocab)
  ndoc <- nrow(get_document(obama))
  expect_equal(dim(tf_direct$tf), c(ndoc, vlen))
  expect_equal(length(tf_direct$idf), vlen)
  expect_equal(dim(tf_direct$tfidf), c(ndoc, vlen))

  tf_direct <- get_tfidf(obama)
  tf_manual <- get_tfidf(get_token(obama), doc_var = "id",
    token_var = "lemma")
  expect_equal(tf_direct, tf_manual)

  tf_direct <- get_tfidf(obama, max_features = 5L)
  expect_equal(ncol(tf_direct$tfidf), 5L)

  vocabulary <- get_tfidf(obama, type = "vocab",
    max_features = 100L)$vocab
  expect_equal(class(vocabulary), "character")
  expect_equal(length(vocabulary), 100L)

  tf_direct <- get_tfidf(obama, max_features = 100L)
  tf_manual <- get_tfidf(obama, vocabulary = vocabulary)
  expect_equal(tf_direct, tf_manual)
})


test_that("testing tidy_pca", {
  obj <- get_tfidf(obama)
  res <- tidy_pca(obj$tfidf)
  expect_equal(class(res), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(res), c("PC1", "PC2"))
  expect_equal(nrow(res), nrow(obj$tfidf))

  res <- tidy_pca(obj$tfidf, k = 5L)
  expect_equal(class(res), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(res), c("PC1", "PC2", "PC3", "PC4", "PC5"))
  expect_equal(nrow(res), nrow(obj$tfidf))

  res <- tidy_pca(obj$tfidf, meta = get_document(obama)[,c("id", "time")])
  expect_equal(class(res), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(res), c("id", "time", "PC1", "PC2"))
  expect_equal(nrow(res), nrow(obj$tfidf))

})



