library(testthat)

# Input has variety of entities, first entry has person and personal pronoun
simple.input.test <- c("There is a person called Julie that went down the lane. She likes bubbles", 
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

simple.with.pronouns.expected <- structure(list(id = c(1L, 1L, 3L, 4L, 4L, 4L), 
                                  entity = c("Julie", "She", "Hong Kong", "two", "Jane", "John"),
                                  entity.type = c("PERSON", "PERSON", "CITY", "NUMBER", "PERSON",
                                                  "PERSON")),
                             class = "data.frame",
                             row.names = c(NA, -6L))

pronouns <- c("he's", "hes", "he is", "He is", "He Is", "she's", "She is")

all.single.entity <- as.character(1:3)

none.expected <- data.frame(id = character(), entity = character(), entity.type = character())

all.single.output <- structure(list(id = 1:3,
                                    entity = c("1", "2", "3"),
                                    entity.type = c("NUMBER", "NUMBER", "NUMBER")),
                               class = "data.frame", row.names = c(NA, -3L))

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
  expect_error(NERAnnotate(),
               "^Java CoreNLP not initialized. Named Entity Recognition cannot be executed.$")
  
  cnlp_init_corenlp_custom(language = "en", mem = "2g", keys = keys, values = values, 
                           corenlp.only = TRUE)
  
  expect_error(simple.output <- NERAnnotate(), NA)
  expect_identical(simple.output, simple.expected)
  
  expect_error(simple.output.with.pronouns <- NERAnnotate(entity.mentions.only = TRUE), NA)
  expect_identical(simple.output.with.pronouns, simple.with.pronouns.expected)
  
  file <- file(tmp.file, "wb")
  writeLines(none.input, con = file)
  close(file)
  
  none.output <- NERAnnotate()
  expect_identical(none.output, none.expected)
  
  file <- file(tmp.file, "wb")
  writeLines(pronouns, con = file)
  close(file)
  
  expect_error(pronoun.output.after.validation <- NERAnnotate(entity.mentions.only = FALSE), NA)
  expect_identical(pronoun.output.after.validation, none.expected)
  
  file <- file(tmp.file, "wb")
  writeLines(all.single.entity, con = file)
  close(file)
  
  expect_error(all.single.entity.output <- NERAnnotate(entity.mentions.only = FALSE), NA)
  expect_identical(all.single.entity.output, all.single.output)
})
