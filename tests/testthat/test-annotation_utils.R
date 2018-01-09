library(cleanNLP)

context("Testing annotation utility functions")

data(obama)
input_dir <- system.file("txt_files", package="cleanNLP")
input_files <- file.path(input_dir,
  c("bush.txt", "clinton.txt", "obama.txt"))

check_spacy_exists <- function() {
  if (!requireNamespace("reticulate")) {
    skip("Python or reticulate not available")
  } else if (!reticulate::py_module_available("spacy")) {
    skip("spaCy module is not installed.")
  }
}

test_that("extract subset of documents from an annotation object", {
  sub_obama <- cnlp_extract_documents(obama, ids = c("doc2009", "doc2012"))
  expect_equal(unique(cnlp_get_token(sub_obama)$id), c("doc2009", "doc2012"))
  expect_equal(unique(cnlp_get_document(sub_obama)$id), c("doc2009", "doc2012"))
  expect_equal(unique(cnlp_get_dependency(sub_obama)$id), c("doc2009", "doc2012"))
  expect_equal(unique(cnlp_get_entity(sub_obama)$id), c("doc2009", "doc2012"))
})

test_that("combine documents", {
  obama1 <- cnlp_extract_documents(obama, ids = c("doc2009", "doc2012"))
  obama2 <- cnlp_extract_documents(obama, ids = c("doc2015"))
  obama12 <- cnlp_combine_documents(obama1, obama2)
  obama3 <- cnlp_extract_documents(obama, ids = c("doc2009", "doc2012", "doc2015"))

  expect_equal(obama12, obama3)
})

test_that("read and write annotations", {
  skip_on_cran()
  check_spacy_exists()

  cnlp_init_spacy(vector_flag = TRUE)
  anno <- cnlp_annotate(input_files, as_strings = FALSE)

  od <- file.path(tempdir(), "test_dir_2")
  cnlp_write_csv(anno, od)

  anno_from_disk <- cnlp_read_csv(od)
  expect_equal(anno, anno_from_disk)
})

