library(cleanNLP)

context("Testing python/spaCy backend")

input_dir <- system.file("txt_files", package="cleanNLP")
input_files <- file.path(input_dir, c("bush.txt", "clinton.txt", "obama.txt"))

check_spacy_exists <- function() {
  if (!requireNamespace("reticulate")) {
    skip("Python or reticulate not available")
  } else if (!reticulate::py_module_available("spacy")) {
    skip("spaCy module is not installed.")
  }
}

test_that("annotation gives error if spaCy is uninitialized", {
  skip_on_cran()
  check_spacy_exists()

  cleanNLP:::.onLoad()
  expect_error(annotate(input_files, backend = "spaCy"),
               "The spaCy backend has not been initialized.")
})

test_that("initialize gives error with bad model name", {
  skip_on_cran()
  check_spacy_exists()

  expect_error(init_spaCy(model_name = "elvish"))
})

test_that("tokens with spaCy", {
  skip_on_cran()
  check_spacy_exists()

  init_spaCy()
  anno <- annotate(input_files)
  token <- get_token(anno)

  expect_equal(class(token), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(token), c("id", "sid", "tid", "word", "lemma",
                                "upos", "pos", "cid"))
  expect_equal(token$id, sort(token$id))
  expect_equal(token$sid[token$id == 0], sort(token$sid[token$id == 0]))
  expect_equal(token$sid[token$id == 1], sort(token$sid[token$id == 1]))
  expect_equal(token$sid[token$id == 2], sort(token$sid[token$id == 2]))

  any_missing <- apply(is.na(token), 2, any)
  expect_true(!any(any_missing[1:3]))
})

test_that("dependency with spaCy", {
  skip_on_cran()
  check_spacy_exists()

  init_spaCy()
  anno <- annotate(input_files)
  dep <- get_dependency(anno)

  expect_equal(class(dep), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(dep), c("id", "sid", "tid", "tid_target", "relation",
                             "relation_full"))
  expect_equal(dep$id, sort(dep$id))

  any_missing <- apply(is.na(dep), 2, any)
  expect_true(!any(any_missing[1:4]))
})

test_that("entity with spaCy", {
  skip_on_cran()
  check_spacy_exists()

  init_spaCy()
  anno <- annotate(input_files)
  ent <- get_entity(anno)

  expect_equal(class(ent), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(ent), c("id", "sid", "tid", "tid_end", "entity_type",
                             "entity"))
  expect_equal(ent$id, sort(ent$id))

  any_missing <- apply(is.na(ent), 2, any)
  expect_true(!any(any_missing[1:5]))
})

test_that("set_spacy_properties", {
  skip_on_cran()
  check_spacy_exists()

  init_spaCy(vector_flag = TRUE)
  anno <- annotate(input_files)
  expect_true(nrow(get_vector(anno)) > 0L)
  expect_equal(ncol(get_vector(anno)), 303L)
  expect_equal(class(get_vector(anno)), "matrix")

  init_spaCy(entity_flag = FALSE)
  anno <- annotate(input_files)
  expect_equal(nrow(get_entity(anno)), 0L)
  expect_equal(nrow(get_vector(anno)), 0L)
})

test_that("annotate options", {
  skip_on_cran()
  check_spacy_exists()

  init_spaCy(vector_flag = FALSE)
  anno <- annotate(input_files, doc_id_offset = 137)
  token <- get_token(anno)
  expect_equal(unique(token$id), 137L:139L)

  anno <- annotate(c("Hi duck.", "Hi bunny.", "Hello goose."), as_strings = TRUE)
  token <- get_token(anno)
  expect_equal(dim(token), c(12L, 8L))

  od <- tempfile()
  anno <- annotate(input_files, output_dir = od)
  anno2 <- read_annotation(od)
  expect_equal(anno, anno2)

  init_spaCy(vector_flag = TRUE)
  od <- file.path(tempdir(), "test_dir")
  anno <- annotate(input_files, output_dir = od)
  anno2 <- read_annotation(od)
  expect_equal(anno, anno2)

  init_spaCy(vector_flag = FALSE)
  od <- tempfile()
  anno <- annotate(input_files, output_dir = od, keep = FALSE)
  expect_error({ anno2 <- read_annotation(od) })

  od <- tempfile()
  anno <- annotate(input_files, output_dir = od, load = FALSE)
  od <- file.path(Sys.glob(od), "")
  expect_equal(anno, od)
})



