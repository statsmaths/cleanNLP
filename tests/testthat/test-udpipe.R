library(cleanNLP)

context("Testing udpipe backend")

input_dir <- system.file("txt_files", package="cleanNLP")
input_files <- file.path(input_dir,
  c("bush.txt", "clinton.txt", "obama.txt"))


test_that("annotation gives error if udpipe is uninitialized", {
  skip_on_cran()

  cleanNLP:::.onLoad()
  expect_error(cnlp_annotate(input_files, backend = "udpipe"),
               "You must initialize udpipe with: init_udpipe_backend()")
})

test_that("initialize gives error with bad model name", {
  skip_on_cran()

  expect_error(cnlp_init_udpipe(model_name = "elvish"))
})

test_that("tokens with udpipe", {
  skip_on_cran()

  cnlp_init_udpipe()
  anno <- cnlp_annotate(input_files, as_strings = FALSE)
  token <- cnlp_get_token(anno)

  expect_equal(class(token), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(token)[1:8], c("id", "sid", "tid", "word",
                               "lemma", "upos", "pos", "cid"))
  expect_equal(token$id, sort(token$id))
  expect_equal(token$sid[token$id == "doc1"],
               sort(token$sid[token$id == "doc1"]))
  expect_equal(token$sid[token$id == "doc2"],
               sort(token$sid[token$id == "doc2"]))
  expect_equal(token$sid[token$id == "doc3"],
               sort(token$sid[token$id == "doc3"]))

  any_missing <- apply(is.na(token), 2, any)
  expect_true(!any(any_missing[1:3]))
})

test_that("dependency with spacy", {
  skip_on_cran()

  cnlp_init_udpipe()
  anno <- cnlp_annotate(input_files, as_strings = FALSE)
  dep <- cnlp_get_dependency(anno)

  expect_equal(class(dep), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(dep), c("id", "sid", "tid", "tid_target",
                             "relation", "relation_full"))
  expect_equal(dep$id, sort(dep$id))

  any_missing <- apply(is.na(dep), 2, any)
  expect_true(!any(any_missing[1:4]))
})



test_that("cnlp_annotate options", {
  skip_on_cran()

  cnlp_init_udpipe()
  anno <- cnlp_annotate(input_files,
                        doc_ids = c("bush", "clinton", "obama"),
                        as_strings = FALSE)
  token <- cnlp_get_token(anno)
  expect_equal(unique(token$id), c("bush", "clinton", "obama"))

})



