library(testthat)

simple.input.test <- c("There is a person called Julie that went down the lane.", 
                       "Toys are fine",
                       "There is trouble brewing in Hong Kong",
                       "There are two people caled Jane and John")

result <- structure(list(id = c("doc1", "doc3", "doc4", "doc4", "doc4"), 
                         sid = c(0L, 1L, 0L, 0L, 1L), 
                         tid = c(6L, 6L, 3L, 6L, 8L), 
                         tid_end = c(6L, 7L, 3L, 6L, 8L),
                         entity_type = c("PERSON", "CITY", "NUMBER", "PERSON", "PERSON"),
                         entity = c("Julie","Hong Kong", "two", "Jane", "John"), 
                         entity_normalized = c(NA, NA, 2, NA, NA)),
                    class = c("tbl_df", "tbl", "data.frame"),
                    row.names = c(NA, -5L))

# If this is throwing errors that you need to download Core NLP then the way to get testthat to 
# find CORENLP is to set CORENLP as a system environment variable with the path to CoreNLP
# CoreNLP directories in the package installation cannot be located by testthat
# Nor found by R CMD check 
# E.g. wget
# 
test_that("get_entity consistency", {
  cnlp_init_corenlp_custom(language = "en", mem = "2g", 
                           keys="annotators", values="tokenize, ssplit, pos, lemma, ner", 
                           verbose = TRUE)
  
  annotated <- cnlp_annotate(simple.input.test, as_strings = TRUE, backend = "coreNLP")
  table.output <- cnlp_get_entity(annotated)
  expect_identical(table.output, result)
})