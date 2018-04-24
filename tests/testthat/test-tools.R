library(cleanNLP)

context("Testing tools for working with textual data")

data(obama)

test_that("testing utils_tfidf", {
  tf_direct <- cnlp_utils_tfidf(obama, type = "all")
  vlen <- length(tf_direct$vocab)
  ndoc <- nrow(cnlp_get_document(obama))
  expect_equal(dim(tf_direct$tf), c(ndoc, vlen))
  expect_equal(length(tf_direct$idf), vlen)
  expect_equal(dim(tf_direct$tfidf), c(ndoc, vlen))

  tf_direct <- cnlp_utils_tfidf(obama)
  tf_manual <- cnlp_utils_tfidf(cnlp_get_token(obama), doc_var = "id",
    token_var = "lemma")
  expect_equal(tf_direct, tf_manual)

  tf_direct <- cnlp_utils_tfidf(obama, max_features = 5L)
  expect_equal(ncol(tf_direct), 5L)

  vocabulary <- cnlp_utils_tfidf(obama, type = "vocab",
    max_features = 100L)
  expect_equal(class(vocabulary), "character")
  expect_equal(length(vocabulary), 100L)

  tf_direct <- cnlp_utils_tfidf(obama, max_features = 100L)
  tf_manual <- cnlp_utils_tfidf(obama, vocabulary = vocabulary)
  expect_equal(tf_direct, tf_manual)
  expect_equal(colnames(tf_direct), vocabulary)
})


test_that("testing tidy_pca", {
  tfidf <- cnlp_utils_tfidf(obama)
  res <- cnlp_utils_pca(tfidf)
  expect_equal(class(res), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(res), c("PC1", "PC2"))
  expect_equal(nrow(res), nrow(tfidf))

  res <- cnlp_utils_pca(tfidf, k = 5L)
  expect_equal(class(res), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(res), c("PC1", "PC2", "PC3", "PC4", "PC5"))
  expect_equal(nrow(res), nrow(tfidf))

  res <- cnlp_utils_pca(tfidf,
                        meta = cnlp_get_document(obama)[,c("id", "time")])
  expect_equal(class(res), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(res), c("id", "time", "PC1", "PC2"))
  expect_equal(nrow(res), nrow(tfidf))

})



