library(cleanNLP)

context("Testing java backend")

input_dir <- system.file("txt_files", package="cleanNLP")
input_files <- file.path(input_dir,
  c("bush.txt", "clinton.txt", "obama.txt"))

# Downloading the files is slow and network intensive; to test download
# coreNLP library to this location and then the tests will run.
lib_loc <- "~/local/core_nlp_files/stanford-corenlp-full-2016-10-31"

check_corenlp_available <- function() {
  if (!requireNamespace("rJava")) {
    skip("rJava or Java not available")
  } else if (!dir.exists(lib_loc)) {
    skip("coreNLP files are not found for testing.")
  }
}

test_that("annotation gives error if coreNLP is uninitialized", {
  skip_on_cran()
  check_corenlp_available()

  cleanNLP:::.onLoad()
  expect_error(run_annotators(input_files, backend = "coreNLP"),
               "The coreNLP backend has not been initialized.")
})

test_that("initialize gives error with bad lib_location", {
  skip_on_cran()
  check_corenlp_available()

  expect_error(init_coreNLP(type = "coreNLP",
    lib_location="/file/not/exists"))
})

test_that("coreNLP; anno_level 0", {
  skip_on_cran()
  check_corenlp_available()

  init_coreNLP("en", anno_level = 0, lib_location = lib_loc)
  anno <- run_annotators(input_files, backend = "coreNLP")

  # check token
  token <- get_token(anno)
  expect_equal(class(token), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(token), c("id", "sid", "tid", "word", "lemma",
                                "upos", "pos", "cid"))
  expect_equal(token$id, sort(token$id))
  expect_equal(token$sid[token$id == 0], sort(token$sid[token$id == 0]))
  expect_equal(token$sid[token$id == 1], sort(token$sid[token$id == 1]))
  expect_equal(token$sid[token$id == 2], sort(token$sid[token$id == 2]))

  any_missing <- apply(is.na(token), 2, any)
  expect_true(!any(any_missing[1:5]))

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

test_that("coreNLP; anno_level 1", {
  skip_on_cran()
  check_corenlp_available()

  init_coreNLP("en", anno_level = 1, lib_location = lib_loc)
  anno <- run_annotators(input_files, backend = "coreNLP")

  # check token
  token <- get_token(anno)
  expect_equal(class(token), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(token), c("id", "sid", "tid", "word", "lemma",
                                "upos", "pos", "cid"))
  expect_equal(token$id, sort(token$id))
  expect_equal(token$sid[token$id == 0], sort(token$sid[token$id == 0]))
  expect_equal(token$sid[token$id == 1], sort(token$sid[token$id == 1]))
  expect_equal(token$sid[token$id == 2], sort(token$sid[token$id == 2]))

  any_missing <- apply(is.na(token), 2, any)
  expect_true(!any(any_missing[1:5]))

  # check document
  doc <- get_document(anno)
  expect_equal(class(doc), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(doc), c("id", "time", "version", "language", "uri"))
  expect_equal(nrow(doc), 3L)

  # check dependency
  dep <- get_dependency(anno)
  expect_equal(class(dep), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(dep), c("id", "sid", "tid", "tid_target",
                             "relation", "relation_full"))
  any_missing <- apply(is.na(dep), 2, any)
  expect_true(!any(any_missing))
  expect_true(nrow(dep) > 0L)

  # check sentence
  sent <- get_sentence(anno)
  expect_equal(class(sent), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(sent), c("id", "sid", "sentiment"))
  any_missing <- apply(is.na(sent), 2, any)
  expect_true(!any(any_missing))
  expect_true(nrow(sent) > 0L)

  # check others empty
  expect_equal(nrow(get_coreference(anno)), 0L)
  expect_equal(nrow(get_entity(anno)), 0L)
})

test_that("coreNLP; anno_level 2", {
  skip_on_cran()
  check_corenlp_available()

  init_coreNLP("en", anno_level = 2, lib_location = lib_loc)
  anno <- run_annotators(input_files, backend = "coreNLP")

  # check token
  token <- get_token(anno)
  expect_equal(class(token), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(token), c("id", "sid", "tid", "word", "lemma",
                                "upos", "pos", "cid"))
  expect_equal(token$id, sort(token$id))
  expect_equal(token$sid[token$id == 0], sort(token$sid[token$id == 0]))
  expect_equal(token$sid[token$id == 1], sort(token$sid[token$id == 1]))
  expect_equal(token$sid[token$id == 2], sort(token$sid[token$id == 2]))

  any_missing <- apply(is.na(token), 2, any)
  expect_true(!any(any_missing[1:5]))

  # check document
  doc <- get_document(anno)
  expect_equal(class(doc), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(doc), c("id", "time", "version", "language", "uri"))
  expect_equal(nrow(doc), 3L)

  # check dependency
  dep <- get_dependency(anno)
  expect_equal(class(dep), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(dep), c("id", "sid", "tid", "tid_target",
                             "relation", "relation_full"))
  any_missing <- apply(is.na(dep), 2, any)
  expect_true(!any(any_missing))
  expect_true(nrow(dep) > 0L)

  # check sentence
  sent <- get_sentence(anno)
  expect_equal(class(sent), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(sent), c("id", "sid", "sentiment"))
  any_missing <- apply(is.na(sent), 2, any)
  expect_true(!any(any_missing))
  expect_true(nrow(sent) > 0L)

  # check NER
  ner <- get_entity(anno)
  expect_equal(class(ner), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(ner), c("id", "sid", "tid", "tid_end", "entity_type",
                             "entity", "entity_normalized"))
  any_missing <- apply(is.na(ner), 2, any)
  expect_true(!any(any_missing[1:6]))
  expect_true(nrow(ner) > 0L)

  # check others empty
  expect_equal(nrow(get_coreference(anno)), 0L)
})

test_that("coreNLP; anno_level 3", {
  skip_on_cran()
  check_corenlp_available()

  init_coreNLP("en", anno_level = 3, lib_location = lib_loc)
  anno <- run_annotators(input_files, backend = "coreNLP")

  # check token
  token <- get_token(anno)
  expect_equal(class(token), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(token), c("id", "sid", "tid", "word", "lemma",
                                "upos", "pos", "cid"))
  expect_equal(token$id, sort(token$id))
  expect_equal(token$sid[token$id == 0], sort(token$sid[token$id == 0]))
  expect_equal(token$sid[token$id == 1], sort(token$sid[token$id == 1]))
  expect_equal(token$sid[token$id == 2], sort(token$sid[token$id == 2]))

  any_missing <- apply(is.na(token), 2, any)
  expect_true(!any(any_missing[1:5]))

  # check document
  doc <- get_document(anno)
  expect_equal(class(doc), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(doc), c("id", "time", "version", "language", "uri"))
  expect_equal(nrow(doc), 3L)

  # check dependency
  dep <- get_dependency(anno)
  expect_equal(class(dep), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(dep), c("id", "sid", "tid", "tid_target",
                             "relation", "relation_full"))
  any_missing <- apply(is.na(dep), 2, any)
  expect_true(!any(any_missing))
  expect_true(nrow(dep) > 0L)

  # check sentence
  sent <- get_sentence(anno)
  expect_equal(class(sent), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(sent), c("id", "sid", "sentiment"))
  any_missing <- apply(is.na(sent), 2, any)
  expect_true(!any(any_missing))
  expect_true(nrow(sent) > 0L)

  # check NER
  ner <- get_entity(anno)
  expect_equal(class(ner), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(ner), c("id", "sid", "tid", "tid_end", "entity_type",
                             "entity", "entity_normalized"))
  any_missing <- apply(is.na(ner), 2, any)
  expect_true(!any(any_missing[1:6]))
  expect_true(nrow(ner) > 0L)

  # check coref
  cr <- get_coreference(anno)
  expect_equal(class(cr), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(cr), c("id", "rid", "mid", "mention", "mention_type",
                            "number", "gender", "animacy", "sid", "tid",
                            "tid_end", "tid_head"))
  any_missing <- apply(is.na(cr), 2, any)
  expect_true(!any(any_missing))
  expect_true(nrow(cr) > 0L)
})

test_that("run_annotators options", {
  skip_on_cran()
  check_corenlp_available()

  init_coreNLP("en", anno_level = 0, lib_location = lib_loc)
  anno <- run_annotators(input_files, doc_id_offset = 137, backend = "coreNLP")
  token <- get_token(anno)
  expect_equal(unique(token$id), 138L:140L)

  anno <- run_annotators(c("Hi duck.", "Hi bunny.", "Hello goose."),
    as_strings = TRUE, backend = "coreNLP")
  token <- get_token(anno)
  expect_equal(dim(token), c(9L, 8L))

  od <- file.path(tempdir(), "test_dir")
  od <- gsub("//", "/", od, fixed = TRUE)
  anno <- run_annotators(input_files, output_dir = od, , backend = "coreNLP")
  anno2 <- read_annotation(od)
  expect_equal(anno, anno2)

  od <- file.path(tempdir(), "test_dir")
  od <- gsub("//", "/", od, fixed = TRUE)
  anno <- run_annotators(input_files, output_dir = od, keep = FALSE,
    backend = "coreNLP")
  expect_error({ anno2 <- read_annotation(od) })

  od <- file.path(tempdir(), "test_dir")
  od <- gsub("//", "/", od, fixed = TRUE)
  anno <- run_annotators(input_files, output_dir = od, load = FALSE,
    backend = "coreNLP")
  od <- file.path(Sys.glob(od), "")
  od <- gsub("//", "/", od, fixed = TRUE)
  anno <- gsub("//", "/", anno, fixed = TRUE)
  expect_equal(anno, od)
})

test_that("download function", {
  skip("only ever run this manually; it download two files > 1GB")
  skip_on_cran()
  check_corenlp_available()

  # download files
  download_core_nlp()

  # test that the files work correctly
  init_coreNLP("en", anno_level = 0, lib_location = lib_loc)
  anno <- run_annotators(input_files)

})


