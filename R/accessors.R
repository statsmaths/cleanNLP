#' Access tokens from an annotation object
#'
#' This function grabs the table of tokens from an annotation object. There
#' is exactly one row for each token found in the raw text. Tokens include
#' words as well as punctuation marks. A token called \code{ROOT} is also
#' added to each sentence; it is particularly useful when interacting with
#' the table of dependencies.
#'
#' @param annotation   an annotation object
#'
#' @return
#'
#'  Returns an object of class \code{c("tbl_df", "tbl", "data.frame")}
#'  containing one row for every token in the corpus. The root of each
#'  sentence is included as its own token.
#'
#'  The returned data frame includes at a minimum the following columns:
#'
#' \itemize{
#'  \item{"id"}{ - integer. Id of the source document.}
#'  \item{"sid"}{ - integer. Sentence id, starting from 0.}
#'  \item{"tid"}{ - integer. Token id, with the root of the sentence starting at 0.}
#'  \item{"word"}{ - character. Raw word in the input text.}
#'  \item{"lemma"}{ - character. Lemmatized form the token.}
#'  \item{"upos"}{ - character. Universal part of speech code.}
#'  \item{"pos"}{ - character. Language-specific part of speech code; uses the Penn Treebank codes.}
#'  \item{"cid"}{ - integer. Character offset at the start of the word in the original document.}
#' }
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @references
#'
#'   Manning, Christopher D., Mihai Surdeanu, John Bauer, Jenny Finkel, Steven J. Bethard, and
#'   David McClosky. 2014. \href{http://nlp.stanford.edu/pubs/StanfordCoreNlp2014.pdf}{The Stanford CoreNLP Natural Language Processing Toolkit}.
#'   In: \emph{Proceedings of the 52nd Annual Meeting of the Association for Computational Linguistics: System Demonstrations, pp. 55-60.}
#'
#'   Kristina Toutanova and Christopher D. Manning. 2000. Enriching the Knowledge Sources Used in a Maximum Entropy Part-of-Speech Tagger.
#'   In: \emph{Proceedings of the Joint SIGDAT Conference on Empirical Methods in Natural Language Processing and Very Large Corpora (EMNLP/VLC-2000), pp. 63-70}.
#'
#'   Kristina Toutanova, Dan Klein, Christopher Manning, and Yoram Singer. 2003. Feature-Rich Part-of-Speech Tagging with a Cyclic Dependency Network.
#'   In: \emph{Proceedings of HLT-NAACL 2003, pp. 252-259.}
#'
#' @examples
#' data(obama)
#'
#' # find average sentence length from each address
#' get_token(obama) %>%
#'   group_by(id, sid) %>%
#'   summarize(sentence_length = max(tid)) %>%
#'   summarize(avg_sentence_length = mean(sentence_length))
#' @export
get_token <- function(annotation) {
  annotation$token
}

#' Access dependencies from an annotation object
#'
#' This function grabs the table of dependencies from an annotation object. These
#' are binary relationships between the tokens of a sentence. Common examples
#' include nominal subject (linking the object of a sentence to a verb), and
#' adjectival modifiers (linking an adjective to a noun). While not included in
#' the underlying data, the function has an option for linking these dependencies
#' to the raw words and lemmas in the table of tokens. Both language-agnostic and
#' language-specific universal dependency types are included in the output.
#'
#' @param annotation   an annotation object
#' @param get_token    logical. Should words and lemmas be attached to the
#'                     returned dependency table.
#' @return
#'
#'  Returns an object of class \code{c("tbl_df", "tbl", "data.frame")}
#'  containing one row for every dependency pair in the corpus.
#'
#'  The returned data frame includes at a minimum the following columns:
#'
#' \itemize{
#'  \item{"id"}{ - integer. Id of the source document.}
#'  \item{"sid"}{ - integer. Sentence id of the source token.}
#'  \item{"tid"}{ - integer. Id of the source token.}
#'  \item{"tid_target"}{ - integer. Id of the source token.}
#'  \item{"relation"}{ - character. Language-agnostic universal dependency type.}
#'  \item{"relation_full"}{ - character. Language specific universal dependency type.}
#' }
#'
#'  If \code{get_token} is set to true, the following columns will also be included:
#'
#' \itemize{
#'  \item{"word"}{ - character. The source word in the raw text.}
#'  \item{"lemma"}{ - character. Lemmatized form of the source word.}
#'  \item{"word_target"}{ - character. The target word in the raw text.}
#'  \item{"lemma_target"}{ - character. Lemmatized form of the target word.}
#' }
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @references
#'
#'   Manning, Christopher D., Mihai Surdeanu, John Bauer, Jenny Finkel, Steven J. Bethard, and
#'   David McClosky. 2014. \href{http://nlp.stanford.edu/pubs/StanfordCoreNlp2014.pdf}{The Stanford CoreNLP Natural Language Processing Toolkit}.
#'   In: \emph{Proceedings of the 52nd Annual Meeting of the Association for Computational Linguistics: System Demonstrations, pp. 55-60.}
#'
#'   Danqi Chen and Christopher D Manning. 2014. A Fast and Accurate Dependency Parser using Neural Networks. In: \emph{Proceedings of EMNLP 2014}
#'
#'   Spence Green, Marie-Catherine de Marneffe, John Bauer, and Christopher D. Manning. 2010. Multiword Expression Identification with Tree
#'   Substitution Grammars: A Parsing tour de force with French. In: \emph{EMNLP 2011}.
#'
#'   Spence Green and Christopher D. Manning. 2010. Better Arabic Parsing: Baselines, Evaluations, and Analysis. In: COLING 2010.
#'
#'   Pi-Chuan Chang, Huihsin Tseng, Dan Jurafsky, and Christopher D. Manning. 2009. Discriminative Reordering with Chinese Grammatical
#'   Relations Features. In: Proceedings of the Third Workshop on Syntax and Structure in Statistical Translation.
#'
#'   Anna Rafferty and Christopher D. Manning. 2008. Parsing Three German Treebanks: Lexicalized and Unlexicalized Baselines.
#'   In: \emph{ACL Workshop on Parsing German.}
#'
#' @examples
#' data(obama)
#'
#' # find the most common noun lemmas that are the syntactic subject of a clause
#' res <- get_dependency(obama, get_token = TRUE) %>%
#'   filter(relation == "nsubj")
#' res$lemma_target %>%
#'   table() %>%
#'   sort(decreasing = TRUE) %>%
#'   head(n = 40)
#'
#' @export
get_dependency <- function(annotation, get_token = FALSE) {
  dep <- annotation$dependency

  # silence R CMD check warnings
  id <- sid <- tid <- word <- lemma <- NULL

  if (get_token) {
    dep <- dplyr::left_join(dep, dplyr::select(annotation$token, id, sid, tid, word, lemma))
    dep <- dplyr::left_join(dep, dplyr::select(annotation$token, id, sid, tid_target = tid,
                                               word_target = word, lemma_target = lemma))
  }

  dep
}

#' Access document meta data from an annotation object
#'
#' @param annotation   an annotation object
#'
#' @return
#'
#'  Returns an object of class \code{c("tbl_df", "tbl", "data.frame")}
#'  containing one row for every document in the corpus.
#'
#'  The returned data frame includes at least the following columns:
#'
#' \itemize{
#'  \item{"id"}{ - integer. Id of the source document.}
#'  \item{"time"}{ - date time. The time at which the parser was run on the text.}
#'  \item{"version"}{ - character. Version of the CoreNLP library used to parse the text.}
#'  \item{"language"}{ - character. Language of the text, in ISO 639-1 format.}
#'  \item{"uri"}{ - character. Description of the raw text location. Set to \code{NA} if parsed from in-memory character vector.}
#' }
#'
#'  Other application specific columns may be included as additional variables.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @references
#'
#'   Manning, Christopher D., Mihai Surdeanu, John Bauer, Jenny Finkel, Steven J. Bethard, and
#'   David McClosky. 2014. \href{http://nlp.stanford.edu/pubs/StanfordCoreNlp2014.pdf}{The Stanford CoreNLP Natural Language Processing Toolkit}.
#'   In: \emph{Proceedings of the 52nd Annual Meeting of the Association for Computational Linguistics: System Demonstrations, pp. 55-60.}
#'
#' @examples
#' data(obama)
#'
#' get_document(obama)
#'
#'
#' @export
get_document <- function(annotation) {
  annotation$document
}

#' Access coreferences from an annotation object
#'
#' Coreferences are collections of expressions that all represent the same
#' person, entity, or thing. For example, the text "Lauren loves dogs. She would walk them all day.",
#' there is a coreference consisting of the token "Lauren" in the first sentence and the token
#' "She" in the second sentence. In the output given from this function, a row is given for
#' any mention of an entity; these can be linked using the \code{rid} key.
#'
#'
#' @param annotation   an annotation object
#'
#' @return
#'  Returns an object of class \code{c("tbl_df", "tbl", "data.frame")}
#'  containing one row for every coreference in the corpus.
#'
#'  The returned data frame includes at least the following columns:
#'
#' \itemize{
#'  \item{"id"}{ - integer. Id of the source document.}
#'  \item{"rid"}{ - integer. Relation ID.}
#'  \item{"mid"}{ - integer. Mention ID; unique to each coreference within a document.}
#'  \item{"mention"}{ - character. The mention as raw words from the text.}
#'  \item{"mention_type"}{ - character. One of "LIST", "NOMINAL", "PRONOMINAL", or "PROPER".}
#'  \item{"number"}{ - character. One of "PLURAL", "SINGULAR", or "UNKNOWN".}
#'  \item{"gender"}{ - character. One of "FEMALE", "MALE", "NEUTRAL", or "UNKNOWN".}
#'  \item{"animacy"}{ - character. One of "ANIMATE", "INANIMATE", or "UNKNOWN".}
#'  \item{"sid"}{ - integer. Sentence id of the coreference.}
#'  \item{"tid"}{ - integer. Token id at the start of the coreference.}
#'  \item{"tid_end"}{ - integer. Token id at the start of the coreference.}
#'  \item{"tid_head"}{ - integer. Token id of the head of the coreference.}
#' }
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @references
#'
#'    Manning, Christopher D., Mihai Surdeanu, John Bauer, Jenny Finkel, Steven J. Bethard, and
#'    David McClosky. 2014. \href{http://nlp.stanford.edu/pubs/StanfordCoreNlp2014.pdf}{The Stanford CoreNLP Natural Language Processing Toolkit}.
#'    In: \emph{Proceedings of the 52nd Annual Meeting of the Association for Computational Linguistics: System Demonstrations, pp. 55-60.}
#'
#'    Marta Recasens, Marie-Catherine de Marneffe, and Christopher Potts.
#'    The Life and Death of Discourse Entities: Identifying Singleton Mentions.
#'    In: \emph{Proceedings of NAACL 2013}.
#'
#'    Heeyoung Lee, Angel Chang, Yves Peirsman, Nathanael Chambers, Mihai Surdeanu and Dan Jurafsky.
#'    Deterministic coreference resolution based on entity-centric, precision-ranked rules.
#'    Computational Linguistics 39(4), 2013.
#'
#'    Heeyoung Lee, Yves Peirsman, Angel Chang, Nathanael Chambers, Mihai Surdeanu, Dan Jurafsky.
#'    Stanford's Multi-Pass Sieve Coreference Resolution System at the CoNLL-2011 Shared Task.
#'    In: \emph{Proceedings of the CoNLL-2011 Shared Task, 2011}.
#'
#'    Karthik Raghunathan, Heeyoung Lee, Sudarshan Rangarajan, Nathanael Chambers, Mihai Surdeanu, Dan Jurafsky, Christopher Manning
#'    A Multi-Pass Sieve for Coreference Resolution. EMNLP-2010, Boston, USA. 2010.
#'
#' @examples
#' data(obama)
#'
#' # how often are references made to males versus female in each speech?
#' coref <- get_coreference(obama)
#' table(coref$gender, coref$id)
#'
#' @export
get_coreference <- function(annotation) {
  annotation$coreference
}

#' Access named entities from an annotation object
#'
#' Named entity recognition attempts to find the mentions of various categories
#' within the corpus of text. Common example include proper references to location
#' (e.g., "Boston", or "England") or people (e.g., "Winston Churchill"), as well as
#' specific dates (e.g., "tomorrow", or "September 19th") times, or numbers.
#'
#' @param annotation   an annotation object
#'
#' @return
#'  Returns an object of class \code{c("tbl_df", "tbl", "data.frame")}
#'  containing one row for every named entity mention in the corpus.
#'
#'  The returned data frame includes the following columns:
#'
#' \itemize{
#'  \item{"id"}{ - integer. Id of the source document.}
#'  \item{"sid"}{ - integer. Sentence id of the entity mention.}
#'  \item{"tid"}{ - integer. Token id at the start of the entity mention.}
#'  \item{"tid_end"}{ - integer. Token id at the end of the entity mention.}
#'  \item{"entity_type"}{ - character. See below from details.}
#'  \item{"entity"}{ - character. Raw words of the named entity in the text.}
#' }
#'
#' @details When using CoreNLP, the default entity types are:
#'  \itemize{
#'    \item{"LOCATION"}{ Countries, cities, states, locations, mountain ranges, bodies of water.}
#'    \item{"PERSON"}{ People, including fictional.}
#'    \item{"ORGANIZATION"}{ Companies, agencies, institutions, etc.}
#'    \item{"MONEY"}{ Monetary values, including unit.}
#'    \item{"PERCENT"}{ Percentages.}
#'    \item{"DATE"}{ Absolute or relative dates or periods.}
#'    \item{"TIME"}{ Times smaller than a day.}
#'  }
#'  For the spaCy engine there is no generic LOCATION, ORGANIZATION is
#'  shortened to ORG, and the following categories are added:
#'  \itemize{
#'    \item{"NORP"}{ Nationalities or religious or political groups.}
#'    \item{"FACILITY"}{ Buildings, airports, highways, bridges, etc.}
#'    \item{"GPE"}{ Countries, cities, states.}
#'    \item{"LOC"}{ Non-GPE locations, mountain ranges, bodies of water.}
#'    \item{"PRODUCT"}{ Objects, vehicles, foods, etc. (Not services.)}
#'    \item{"EVENT"}{ Named hurricanes, battles, wars, sports events, etc.}
#'    \item{"WORK_OF_ART"}{ Titles of books, songs, etc.}
#'    \item{"LANGUAGE"}{ Any named language.}
#'    \item{"QUANTITY"}{ Measurements, as of weight or distance.}
#'    \item{"ORDINAL"}{ "first", "second", etc.}
#'    \item{"CARDINAL"}{ Numerals that do not fall under another type.}
#'  }
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @references
#'
#'   Manning, Christopher D., Mihai Surdeanu, John Bauer, Jenny Finkel, Steven J. Bethard, and
#'   David McClosky. 2014. \href{http://nlp.stanford.edu/pubs/StanfordCoreNlp2014.pdf}{The Stanford CoreNLP Natural Language Processing Toolkit}.
#'   In: \emph{Proceedings of the 52nd Annual Meeting of the Association for Computational Linguistics: System Demonstrations, pp. 55-60.}
#'
#'   Jenny Rose Finkel, Trond Grenager, and Christopher Manning. 2005. Incorporating Non-local Information into Information Extraction Systems by
#'   Gibbs Sampling. In: \emph{Proceedings of the 43nd Annual Meeting of the Association for Computational Linguistics (ACL 2005), pp. 363-370.}
#'
#' @examples
#' data(obama)
#'
#' # what are the most common entity types used in the addresses?
#' get_entity(obama)$entity_type %>%
#'  table()
#'
#' # what are the most common locations mentioned?
#' res <- get_entity(obama) %>%
#'   filter(entity_type == "LOCATION")
#' res$entity %>%
#'   table() %>%
#'   sort(decreasing = TRUE) %>%
#'   head(n = 25)
#'
#' # what are the most common organizations mentioned?
#' res <- get_entity(obama) %>%
#'   filter(entity_type == "ORGANIZATION")
#' res$entity %>%
#'   table() %>%
#'   sort(decreasing = TRUE) %>%
#'   head(n = 25)
#'
#' @export
get_entity <- function(annotation) {
  annotation$entity
}

#' Access sentence-level annotations
#'
#' @param annotation   an annotation object
#'
#' @return
#'  Returns an object of class \code{c("tbl_df", "tbl", "data.frame")}
#'  containing one row for every sentence in the corpus.
#'
#'  The returned data frame includes at a minimum following columns:
#'
#' \itemize{
#'  \item{"id"}{ - integer. Id of the source document.}
#'  \item{"sid"}{ - integer. Sentence id.}
#' }
#'
#' The coreNLP backend also currently returns a column "sentiment" that
#' gives a score from 0 (worst) to 4 (best) for how positive the
#' tone of the sentence is predicted to be.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @references
#'
#'   Manning, Christopher D., Mihai Surdeanu, John Bauer, Jenny Finkel, Steven J. Bethard, and
#'   David McClosky. 2014. \href{http://nlp.stanford.edu/pubs/StanfordCoreNlp2014.pdf}{The Stanford CoreNLP Natural Language Processing Toolkit}.
#'   In: \emph{Proceedings of the 52nd Annual Meeting of the Association for Computational Linguistics: System Demonstrations, pp. 55-60.}
#'
#'   Socher, Richard, et al. "Recursive deep models for semantic compositionality over a sentiment treebank." Proceedings of the conference on
#'   empirical methods in natural language processing (EMNLP). Vol. 1631. 2013.
#'
#' @examples
#'
#' # how do the predicted sentiment scores change across the years?
#' get_sentence(obama) %>%
#'   group_by(id) %>%
#'   summarize(mean(sentiment), se = sd(sentiment) / sqrt(n()))
#'
#' @export
get_sentence <- function(annotation) {
  annotation$sentence
}


#' Access word embedding vector from an annotation object
#'
#' Word embeddings map each lemma or token into a high-dimensional
#' vector space. The implementation here uses a 300-dimensional
#' space. Only available with the spaCy parser.
#'
#' @param annotation   an annotation object
#'
#' @return
#'  Returns a matrix containing one row for every triple found
#'  in the corpus, or \code{NULL} if not embeddings are present
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @references
#'
#' Pennington, Jeffrey, Richard Socher, and Christopher D. Manning.
#' "Glove: Global Vectors for Word Representation." EMNLP. Vol. 14. 2014.
#'
#' @export
get_vector <- function(annotation) {
  annotation$vector
}
