library(testthat)

# Input has variety of entities
simple.input.test <- c("There is a person called Julie that went down the lane.",  # Person, Julie,
                       "Toys are fine", #No entities
                       "There is trouble brewing in Hong Kong", #Location Hong Kong
                       "There are two people caled Jane and John") 
# Last input variable has three entities, the Number two and 2 people, Jane and John

# Test result when no entities are present
none.input <- c("There is no entity here",
                "Nor here")

simple.expected <- structure(list(id = c(1L, 3L, 4L, 4L, 4L), 
                                  entity = c("Julie", "Hong Kong", "two", "Jane", "John"),
                                  entity.type = c("PERSON", "CITY", "NUMBER", "PERSON", "PERSON")),
                             class = "data.frame",
                             row.names = c(NA, -5L))

none.expected <- data.frame(id = character(), entity = character(), entity.type = character())

# If this is throwing errors that you need to download Core NLP then the way to get testthat to 
# find CORENLP is to set CORENLP as a system environment variable with the path to CoreNLP
# CoreNLP directories in the package installation cannot be located by testthat
# Nor found by R CMD check 
# E.g. cleanNLP::cnlp_download_corenlp()
test_that("NERAnnotate consistency", {
  
  tmp.file <- tempfile()
  
  file <- file(tmp.file, "wb")
  writeLines(simple.input.test, con = file)
  close(file)
  
  keys <- c("ssplit.eolonly", "annotators", "outputFormat", "file", "outputDirectory")
  values <- c("true", "tokenize,ssplit,pos,lemma,ner", "json", tmp.file, dirname(tmp.file))
  
  # Expect error if NERAnnotate is called before corenlp is initialised.
  expect_error(NERAnnotate(tmp.file),
               "^Java CoreNLP not initialized. Named Entity Recognition cannot be executed.$")
  
  cnlp_init_corenlp_custom(language = "en", mem = "2g", keys = keys, values = values)
  
  simple.output <- NERAnnotate(tmp.file)
  expect_identical(simple.output, simple.expected)
  
  file <- file(tmp.file, "wb")
  writeLines(none.input, con = file)
  close(file)
  
  none.output <- NERAnnotate(tmp.file)
  expect_identical(none.output, none.expected)
  
})