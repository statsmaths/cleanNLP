library(cleanNLP)

context("Testing R/tokenizers backend")

input_dir <- system.file("txt_files", package="cleanNLP")
input_files <- file.path(input_dir, c("bush.txt", "clinton.txt", "obama.txt"))

test_that("annotation uses tokenizers by default", {
  cleanNLP:::.onLoad()
  init_tokenizers()
  anno1 <- annotate(input_files)
  anno2 <- annotate(input_files, backend = "tokenizers")
  anno1$document$time <- anno2$document$time # the times of course will not match
  expect_equal(anno1, anno2)
})


test_that("output of tokenizers", {
  init_tokenizers()
  anno <- annotate(input_files)

  # check tokens
  token <- get_token(anno)

  expect_equal(class(token), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(token), c("id", "sid", "tid", "word", "lemma",
                                "upos", "pos", "cid"))
  expect_equal(token$id, sort(token$id))
  expect_equal(token$sid[token$id == 0], sort(token$sid[token$id == 0]))
  expect_equal(token$sid[token$id == 1], sort(token$sid[token$id == 1]))
  expect_equal(token$sid[token$id == 2], sort(token$sid[token$id == 2]))

  any_missing <- apply(is.na(token), 2, any)
  expect_true(!any(any_missing[1:4]))
  all_missing <- apply(is.na(token), 2, all)
  expect_true(all(all_missing[5:8]))

  # check document
  doc <- get_document(anno)
  expect_equal(class(doc), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(doc), c("id", "time", "version", "language", "uri"))
  expect_equal(nrow(doc), 3L)

  # check others empty
  expect_equal(nrow(get_dependency(anno)), 0L)
  expect_equal(nrow(get_coreference(anno)), 0L)
  expect_equal(nrow(get_entity(anno)), 0L)
  expect_equal(nrow(get_sentence(anno)), 0L)
})


test_that("annotate options", {
  init_tokenizers()

  anno <- annotate(input_files, doc_id_offset = 137, backend = "tokenizers")
  token <- get_token(anno)
  expect_equal(unique(token$id), 137L:139L)

  anno <- annotate(c("Hi duck.", "Hi bunny.", "Hello goose."), as_strings = TRUE, backend = "tokenizers")
  token <- get_token(anno)
  expect_equal(dim(token), c(9L, 8L))

  od <- file.path(tempdir(), "test_dir")
  anno <- annotate(input_files, output_dir = od, backend = "tokenizers")
  anno2 <- read_annotation(od)
  expect_equal(anno, anno2)

  od <- file.path(tempdir(), "test_dir")
  anno <- annotate(input_files, output_dir = od, load = FALSE, backend = "tokenizers")
  od <- file.path(Sys.glob(od), "")
  expect_equal(anno, od)
})



