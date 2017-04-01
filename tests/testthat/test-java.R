library(cleanNLP)

context("Testing java backend")

input_dir <- system.file("txt_files", package="cleanNLP")
input_files <- file.path(input_dir, c("bush.txt", "clinton.txt", "obama.txt"))

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
  expect_error(annotate(input_files, backend = "coreNLP"),
               "The coreNLP backend has not been initialized.")
})

test_that("initialize gives error with bad lib_location", {
  skip_on_cran()
  check_corenlp_available()

  expect_error(init_clean_nlp(type = "coreNLP", lib_location="/file/not/exists"))
})

test_that("coreNLP; speed code 0", {
  skip_on_cran()
  check_corenlp_available()

  set_java_properties("en", speed = 0)
  init_clean_nlp(type = "coreNLP", lib_location = lib_loc)
  anno <- annotate(input_files, backend = "coreNLP")

  # check token
  token <- get_token(anno)
  expect_equal(class(token), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(token), c("id", "sid", "tid", "word", "lemma",
                                "upos", "pos", "speaker", "wiki", "cid",
                                "cid_end"))
  expect_equal(token$id, sort(token$id))
  expect_equal(token$sid[token$id == 0], sort(token$sid[token$id == 0]))
  expect_equal(token$sid[token$id == 1], sort(token$sid[token$id == 1]))
  expect_equal(token$sid[token$id == 2], sort(token$sid[token$id == 2]))

  any_missing <- apply(is.na(token), 2, any)
  expect_true(!any(any_missing[1:3]))
  all_missing <- apply(is.na(token), 2, all)
  expect_true(all_missing[8]) # speaker
  expect_true(all_missing[9]) # wiki

  # check document
  doc <- get_document(anno)
  expect_equal(class(doc), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(doc), c("id", "time", "version", "language", "uri"))
  expect_equal(nrow(doc), 3L)

  # check others empty
  expect_equal(nrow(get_dependency(anno)), 0L)
  expect_equal(nrow(get_coreference(anno)), 0L)
  expect_equal(nrow(get_entity(anno)), 0L)
  expect_equal(nrow(get_triple(anno)), 0L)
  expect_equal(nrow(get_sentiment(anno)), 0L)
})

test_that("coreNLP; speed code 1", {
  skip_on_cran()
  check_corenlp_available()

  set_java_properties("en", speed = 1)
  init_clean_nlp(type = "coreNLP", lib_location = lib_loc)
  anno <- annotate(input_files, backend = "coreNLP")

  # check token
  token <- get_token(anno)
  expect_equal(class(token), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(token), c("id", "sid", "tid", "word", "lemma",
                                "upos", "pos", "speaker", "wiki", "cid",
                                "cid_end"))
  expect_equal(token$id, sort(token$id))
  expect_equal(token$sid[token$id == 0], sort(token$sid[token$id == 0]))
  expect_equal(token$sid[token$id == 1], sort(token$sid[token$id == 1]))
  expect_equal(token$sid[token$id == 2], sort(token$sid[token$id == 2]))

  any_missing <- apply(is.na(token), 2, any)
  expect_true(!any(any_missing[1:3]))
  all_missing <- apply(is.na(token), 2, all)
  expect_true(all_missing[8]) # speaker
  expect_true(all_missing[9]) # wiki
  expect_true(nrow(token) > 0L)

  # check document
  doc <- get_document(anno)
  expect_equal(class(doc), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(doc), c("id", "time", "version", "language", "uri"))
  expect_equal(nrow(doc), 3L)

  # check dependency
  dep <- get_dependency(anno)
  expect_equal(class(dep), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(dep), c("id", "sid", "tid", "sid_target", "tid_target",
                             "relation", "relation_full"))
  any_missing <- apply(is.na(dep), 2, any)
  expect_true(!any(any_missing))
  expect_true(nrow(dep) > 0L)

  # check sentiment
  sent <- get_sentiment(anno)
  expect_equal(class(sent), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(sent), c("id", "sid", "pred_class", "p0", "p1", "p2", "p3", "p4"))
  any_missing <- apply(is.na(sent), 2, any)
  expect_true(!any(any_missing))
  expect_true(nrow(sent) > 0L)

  # check others empty
  expect_equal(nrow(get_coreference(anno)), 0L)
  expect_equal(nrow(get_entity(anno)), 0L)
  expect_equal(nrow(get_triple(anno)), 0L)
})

test_that("coreNLP; speed code 2", {
  skip_on_cran()
  check_corenlp_available()

  set_java_properties("en", speed = 2)
  init_clean_nlp(type = "coreNLP", lib_location = lib_loc)
  anno <- annotate(input_files, backend = "coreNLP")

  # check token
  token <- get_token(anno)
  expect_equal(class(token), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(token), c("id", "sid", "tid", "word", "lemma",
                                "upos", "pos", "speaker", "wiki", "cid",
                                "cid_end"))
  expect_equal(token$id, sort(token$id))
  expect_equal(token$sid[token$id == 0], sort(token$sid[token$id == 0]))
  expect_equal(token$sid[token$id == 1], sort(token$sid[token$id == 1]))
  expect_equal(token$sid[token$id == 2], sort(token$sid[token$id == 2]))

  any_missing <- apply(is.na(token), 2, any)
  expect_true(!any(any_missing[1:3]))
  all_missing <- apply(is.na(token), 2, all)
  expect_true(all_missing[8]) # speaker
  expect_true(all_missing[9]) # wiki
  expect_true(nrow(token) > 0L)

  # check document
  doc <- get_document(anno)
  expect_equal(class(doc), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(doc), c("id", "time", "version", "language", "uri"))
  expect_equal(nrow(doc), 3L)

  # check dependency
  dep <- get_dependency(anno)
  expect_equal(class(dep), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(dep), c("id", "sid", "tid", "sid_target", "tid_target",
                             "relation", "relation_full"))
  any_missing <- apply(is.na(dep), 2, any)
  expect_true(!any(any_missing))
  expect_true(nrow(dep) > 0L)

  # check sentiment
  sent <- get_sentiment(anno)
  expect_equal(class(sent), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(sent), c("id", "sid", "pred_class", "p0", "p1", "p2", "p3", "p4"))
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
  expect_equal(nrow(get_triple(anno)), 0L)
})


test_that("coreNLP; speed code 4", {
  skip_on_cran()
  check_corenlp_available()

  set_java_properties("en", speed = 4)
  init_clean_nlp(type = "coreNLP", lib_location = lib_loc)
  anno <- annotate(input_files, backend = "coreNLP")

  # check token
  token <- get_token(anno)
  expect_equal(class(token), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(token), c("id", "sid", "tid", "word", "lemma",
                                "upos", "pos", "speaker", "wiki", "cid",
                                "cid_end"))
  expect_equal(token$id, sort(token$id))
  expect_equal(token$sid[token$id == 0], sort(token$sid[token$id == 0]))
  expect_equal(token$sid[token$id == 1], sort(token$sid[token$id == 1]))
  expect_equal(token$sid[token$id == 2], sort(token$sid[token$id == 2]))

  any_missing <- apply(is.na(token), 2, any)
  expect_true(!any(any_missing[1:3]))
  all_missing <- apply(is.na(token), 2, all)
  expect_true(all_missing[8]) # speaker
  expect_true(all_missing[9]) # wiki
  expect_true(nrow(token) > 0L)

  # check document
  doc <- get_document(anno)
  expect_equal(class(doc), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(doc), c("id", "time", "version", "language", "uri"))
  expect_equal(nrow(doc), 3L)

  # check dependency
  dep <- get_dependency(anno)
  expect_equal(class(dep), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(dep), c("id", "sid", "tid", "sid_target", "tid_target",
                             "relation", "relation_full"))
  any_missing <- apply(is.na(dep), 2, any)
  expect_true(!any(any_missing))
  expect_true(nrow(dep) > 0L)

  # check sentiment
  sent <- get_sentiment(anno)
  expect_equal(class(sent), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(sent), c("id", "sid", "pred_class", "p0", "p1", "p2", "p3", "p4"))
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

  # check triples
  tri <- get_triple(anno)
  expect_equal(class(tri), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(tri), c("id", "subject", "object", "relation", "confidence", "be_prefix",
                             "be_suffix", "of_suffix", "tmod", "sid", "tid_subject", "tid_subject_end",
                             "tid_object", "tid_object_end", "tid_relation", "tid_relation_end"))
  any_missing <- apply(is.na(tri), 2, any)
  expect_true(!any(any_missing))
  expect_true(nrow(tri) > 0L)

  # check others empty
  expect_equal(nrow(get_coreference(anno)), 0L)
})


test_that("coreNLP; speed code 5", {
  skip_on_cran()
  check_corenlp_available()

  set_java_properties("en", speed = 5)
  init_clean_nlp(type = "coreNLP", lib_location = lib_loc)
  anno <- annotate(input_files, backend = "coreNLP")

  # check token
  token <- get_token(anno)
  expect_equal(class(token), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(token), c("id", "sid", "tid", "word", "lemma",
                                "upos", "pos", "speaker", "wiki", "cid",
                                "cid_end"))
  expect_equal(token$id, sort(token$id))
  expect_equal(token$sid[token$id == 0], sort(token$sid[token$id == 0]))
  expect_equal(token$sid[token$id == 1], sort(token$sid[token$id == 1]))
  expect_equal(token$sid[token$id == 2], sort(token$sid[token$id == 2]))

  any_missing <- apply(is.na(token), 2, any)
  expect_true(!any(any_missing[c(1:3)]))
  all_missing <- apply(is.na(token), 2, all)
  expect_true(all_missing[9]) # wiki
  expect_true(nrow(token) > 0L)

  # check document
  doc <- get_document(anno)
  expect_equal(class(doc), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(doc), c("id", "time", "version", "language", "uri"))
  expect_equal(nrow(doc), 3L)

  # check dependency
  dep <- get_dependency(anno)
  expect_equal(class(dep), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(dep), c("id", "sid", "tid", "sid_target", "tid_target",
                             "relation", "relation_full"))
  any_missing <- apply(is.na(dep), 2, any)
  expect_true(!any(any_missing))
  expect_true(nrow(dep) > 0L)

  # check sentiment
  sent <- get_sentiment(anno)
  expect_equal(class(sent), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(sent), c("id", "sid", "pred_class", "p0", "p1", "p2", "p3", "p4"))
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

  # check others empty
  expect_equal(nrow(get_triple(anno)), 0L)
})

test_that("annotate options", {
  skip_on_cran()
  check_corenlp_available()

  set_java_properties("en", speed = 0)
  init_clean_nlp(type = "spaCy")
  anno <- annotate(input_files, doc_id_offset = 137, backend = "coreNLP")
  token <- get_token(anno)
  expect_equal(unique(token$id), 137L:139L)

  anno <- annotate(c("Hi duck.", "Hi bunny.", "Hello goose."), as_strings = TRUE, backend = "coreNLP")
  token <- get_token(anno)
  expect_equal(dim(token), c(12L, 11L))

  od <- file.path(tempdir(), "test_dir")
  anno <- annotate(input_files, output_dir = od, , backend = "coreNLP")
  anno2 <- read_annotation(od)
  expect_equal(anno, anno2)

  od <- file.path(tempdir(), "test_dir")
  anno <- annotate(input_files, output_dir = od, keep = FALSE, backend = "coreNLP")
  expect_error({ anno2 <- read_annotation(od) })

  od <- file.path(tempdir(), "test_dir")
  anno <- annotate(input_files, output_dir = od, load = FALSE, backend = "coreNLP")
  od <- file.path(Sys.glob(od), "")
  expect_equal(anno, od)
})

test_that("download function", {
  skip("only ever run this manually; it download two files > 1GB")
  skip_on_cran()
  check_corenlp_available()

  # download files
  download_core_nlp()

  # test that the files work correctly
  set_java_properties("en", speed = 0)
  init_clean_nlp(type = "coreNLP")
  anno <- annotate(input_files)

})


