library(cleanNLP)

context("Testing annotation utility functions")

data(obama)
input_dir <- system.file("txt_files", package="cleanNLP")
input_files <- file.path(input_dir, c("bush.txt", "clinton.txt", "obama.txt"))

check_spacy_exists <- function() {
  if (!requireNamespace("reticulate")) {
    skip("Python or reticulate not available")
  } else if (!reticulate::py_module_available("spacy")) {
    skip("spaCy module is not installed.")
  }
}

test_that("extract subset of documents from an annotation object", {
  sub_obama <- extract_documents(obama, ids = c(1L, 4L))
  expect_equal(unique(get_token(sub_obama)$id), c(1L, 4L))
  expect_equal(unique(get_document(sub_obama)$id), c(1L, 4L))
  expect_equal(unique(get_dependency(sub_obama)$id), c(1L, 4L))
  expect_equal(unique(get_coreference(sub_obama)$id), c(1L, 4L))
  expect_equal(unique(get_sentence(sub_obama)$id), c(1L, 4L))
  expect_equal(unique(get_entity(sub_obama)$id), c(1L, 4L))
})

test_that("reset document ids", {
  sub_obama <- extract_documents(obama, ids = c(1L, 4L))
  sub_obama <- doc_id_reset(sub_obama)

  expect_equal(unique(get_token(sub_obama)$id), c(0L, 1L))
  expect_equal(unique(get_document(sub_obama)$id), c(0L, 1L))
  expect_equal(unique(get_dependency(sub_obama)$id), c(0L, 1L))
  expect_equal(unique(get_coreference(sub_obama)$id), c(0L, 1L))
  expect_equal(unique(get_sentence(sub_obama)$id), c(0L, 1L))
  expect_equal(unique(get_entity(sub_obama)$id), c(0L, 1L))

  sub_obama <- extract_documents(obama, ids = c(1L, 4L))
  sub_obama <- doc_id_reset(sub_obama, start_id = 100L)

  expect_equal(unique(get_token(sub_obama)$id), c(100L, 101L))
  expect_equal(unique(get_document(sub_obama)$id), c(100L, 101L))
  expect_equal(unique(get_dependency(sub_obama)$id), c(100L, 101L))
  expect_equal(unique(get_coreference(sub_obama)$id), c(100L, 101L))
  expect_equal(unique(get_sentence(sub_obama)$id), c(100L, 101L))
  expect_equal(unique(get_entity(sub_obama)$id), c(100L, 101L))
})

test_that("combine documents", {
  obama1 <- extract_documents(obama, ids = c(1L, 4L))
  obama2 <- extract_documents(obama, ids = c(7L))
  obama12 <- combine_documents(obama1, obama2)
  obama3 <- extract_documents(obama, ids = c(1L, 4L, 7L))
  obama3 <- doc_id_reset(obama3)

  expect_equal(obama12, obama3)
})

test_that("read and write annotations", {
  skip_on_cran()
  check_spacy_exists()

  init_spaCy(vector_flag = TRUE)
  anno <- annotate(input_files)

  od <- file.path(tempdir(), "test_dir_2")
  write_annotation(anno, od)

  anno_from_disk <- read_annotation(od)
  expect_equal(anno, anno_from_disk)
})

