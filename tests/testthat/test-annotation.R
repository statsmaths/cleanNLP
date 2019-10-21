library(cleanNLP)

context("Testing annotation backends")

data(un)

test_that("testing stringi", {
  cnlp_init_stringi()
  anno <- cnlp_annotate(un, verbose=FALSE)
  expect_equal(names(anno), c("token", "document"))
})

test_that("testing spacy", {
  testthat::skip_on_cran()

  cnlp_init_spacy()
  anno <- cnlp_annotate(un, verbose=FALSE)
  expect_equal(names(anno), c("token", "entity", "document"))
  expect_equal(names(anno$token), c("id", "sid", "tid", "token", "lemma", "upos", "xpos", "tid_source", "relation"))

})

test_that("testing udpipe", {
  testthat::skip_on_cran()

  cnlp_init_udpipe()
  anno <- cnlp_annotate(un, verbose=FALSE)
  expect_equal(names(anno), c("token", "document"))
  expect_equal(names(anno$token), c("id", "sid", "tid", "token", "lemma", "upos", "xpos", "feats", "tid_source", "relation"))

})

test_that("testing corenlp", {
  testthat::skip_on_cran()

  cnlp_init_corenlp()
  anno <- cnlp_annotate(un, verbose=FALSE)
  expect_equal(names(anno), c("token", "document"))
  expect_equal(names(anno$token), c("id", "sid", "tid", "token", "lemma", "upos", "xpos", "feats", "tid_source", "relation"))

})
