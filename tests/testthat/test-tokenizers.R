library(cleanNLP)

context("Testing R/tokenizers backend")

input_dir <- system.file("txt_files", package="cleanNLP")
input_files <- file.path(input_dir,
  c("bush.txt", "clinton.txt", "obama.txt"))

test_that("annotation uses tokenizers by default", {
  cleanNLP:::.onLoad()
  cnlp_init_tokenizers()
  anno1 <- cnlp_annotate(input_files, as_strings = FALSE)
  anno2 <- cnlp_annotate(input_files, backend = "tokenizers", as_strings = FALSE)

  # the times of course will not match
  anno1$document$time <- anno2$document$time
  expect_equal(anno1, anno2)
})


test_that("output of tokenizers", {
  cnlp_init_tokenizers()
  anno <- cnlp_annotate(input_files, as_strings = FALSE)

  # check tokens
  token <- cnlp_get_token(anno)

  expect_equal(class(token), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(token), c("id", "sid", "tid", "word", "lemma",
                                "upos", "pos", "cid"))
  expect_equal(token$id, sort(token$id))
  expect_equal(token$sid[token$id == "doc1"], sort(token$sid[token$id == "doc1"]))
  expect_equal(token$sid[token$id == "doc2"], sort(token$sid[token$id == "doc2"]))
  expect_equal(token$sid[token$id == "doc3"], sort(token$sid[token$id == "doc3"]))

  any_missing <- apply(is.na(token), 2, any)
  expect_true(!any(any_missing[c(1:4,8)]))
  all_missing <- apply(is.na(token), 2, all)
  expect_true(all(all_missing[6:7]))

  # check document
  doc <- cnlp_get_document(anno)
  expect_equal(class(doc), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(doc), c("id", "time", "version", "language", "uri"))
  expect_equal(nrow(doc), 3L)

  # check others empty
  expect_equal(nrow(cnlp_get_dependency(anno)), 0L)
  expect_equal(nrow(cnlp_get_coreference(anno)), 0L)
  expect_equal(nrow(cnlp_get_entity(anno)), 0L)
  expect_equal(nrow(cnlp_get_sentence(anno)), 0L)
})


test_that("run_annotators options", {
  cnlp_init_tokenizers()

  anno <- cnlp_annotate(input_files, doc_ids = c("bush", "clinton", "obama"),
                         backend = "tokenizers",
                         as_strings = FALSE)
  token <- cnlp_get_token(anno)
  expect_equal(unique(token$id), c("bush", "clinton", "obama"))

  anno <- cnlp_annotate(c("Hi duck.", "Hi bunny.", "Hello goose."),
    as_strings = TRUE, backend = "tokenizers")
  token <- cnlp_get_token(anno)
  expect_equal(dim(token), c(9L, 8L))
})



