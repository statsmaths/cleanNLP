library(cleanNLP)

context("Testing python/spacy backend")

input_dir <- system.file("txt_files", package="cleanNLP")
input_files <- file.path(input_dir,
  c("bush.txt", "clinton.txt", "obama.txt"))

check_spacy_exists <- function() {
  if (!requireNamespace("reticulate")) {
    skip("Python or reticulate not available")
  } else if (!reticulate::py_module_available("spacy")) {
    skip("spacy module is not installed.")
  }
}

test_that("annotation gives error if spacy is uninitialized", {
  skip_on_cran()
  check_spacy_exists()

  cleanNLP:::.onLoad()
  expect_error(cnlp_annotate(input_files, backend = "spacy"),
               "You must initialize spacy with: init_spacy_backend()")
})

test_that("initialize gives error with bad model name", {
  skip_on_cran()
  check_spacy_exists()

  expect_error(cnlp_init_spacy(model_name = "elvish"))
})

test_that("tokens with spacy", {
  skip_on_cran()
  check_spacy_exists()

  cnlp_init_spacy()
  anno <- cnlp_annotate(input_files, as_strings = FALSE)
  token <- cnlp_get_token(anno)

  expect_equal(class(token), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(token), c("id", "sid", "tid", "word",
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
  check_spacy_exists()

  cnlp_init_spacy()
  anno <- cnlp_annotate(input_files, as_strings = FALSE)
  dep <- cnlp_get_dependency(anno)

  expect_equal(class(dep), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(dep), c("id", "sid", "tid", "tid_target",
                             "relation", "relation_full"))
  expect_equal(dep$id, sort(dep$id))

  any_missing <- apply(is.na(dep), 2, any)
  expect_true(!any(any_missing[1:4]))
})

test_that("entity with spacy", {
  skip_on_cran()
  check_spacy_exists()

  cnlp_init_spacy()
  anno <- cnlp_annotate(input_files, as_strings = FALSE)
  ent <- cnlp_get_entity(anno)

  expect_equal(class(ent), c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(ent), c("id", "sid", "tid", "tid_end",
                             "entity_type", "entity"))
  expect_equal(ent$id, sort(ent$id))

  any_missing <- apply(is.na(ent), 2, any)
  expect_true(!any(any_missing[1:5]))
})

test_that("cnlp_init_spacy", {
  skip_on_cran()
  check_spacy_exists()

  cnlp_init_spacy(vector_flag = TRUE)
  anno <- cnlp_annotate(input_files, as_strings = FALSE)
  expect_true(nrow(cnlp_get_vector(anno)) > 0L)
  expect_equal(ncol(cnlp_get_vector(anno)), 384L)
  expect_equal(class(cnlp_get_vector(anno)), "matrix")

  cnlp_init_spacy(entity_flag = FALSE)
  anno <- cnlp_annotate(input_files, as_strings = FALSE)
  expect_equal(nrow(cnlp_get_entity(anno)), 0L)
  expect_equal(nrow(cnlp_get_vector(anno)), 0L)
  expect_equal(ncol(cnlp_get_vector(anno)), 0L)
})

test_that("cnlp_annotate options", {
  skip_on_cran()
  check_spacy_exists()

  cnlp_init_spacy(vector_flag = FALSE)
  anno <- cnlp_annotate(input_files,
                        doc_ids = c("bush", "clinton", "obama"),
                        as_strings = FALSE)
  token <- cnlp_get_token(anno)
  expect_equal(unique(token$id), c("bush", "clinton", "obama"))

  anno <- cnlp_annotate(c("Hi duck.", "Hi bunny.", "Hello goose."),
    as_strings = TRUE)
  token <- cnlp_get_token(anno)
  expect_equal(dim(token), c(9L, 8L))

})



