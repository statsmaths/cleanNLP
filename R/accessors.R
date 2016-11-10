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
#'  The returned data frame includes the following columns:
#'
#' \itemize{
#'  \item{"id"}{ - integer. Id of the source document.}
#'  \item{"sid"}{ - integer. Sentence id, starting from 0.}
#'  \item{"tid"}{ - integer. Token id, with the root of the sentence starting at 0.}
#'  \item{"word"}{ - character. Raw word in the input text.}
#'  \item{"lemma"}{ - character. Lemmatized form the token.}
#'  \item{"upos"}{ - character. Universal part of speech code.}
#'  \item{"pos"}{ - character. Language-specific part of speech code; uses the Penn Treebank codes.}
#'  \item{"speaker"}{ - character. Identity of the speaker, with \code{PER0} denoting the narratorial voice.}
#'  \item{"wiki"}{ - character. Link to Wikipedia entity.}
#'  \item{"cid"}{ - integer. Character offset at the start of the word in the original document.}
#'  \item{"cid_end"}{ - integer. Character offset pointing one past the character at the end of the word.}
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
#'  \item{"sid_target"}{ - integer. Sentence id of the source token.}
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
#' get_dependency(obama, get_token = TRUE) %>%
#'   filter(relation == "nsubj") %>%
#'   use_series(lemma_target) %>%
#'   table() %>%
#'   sort(decreasing = TRUE) %>%
#'   head(n = 40)
#'
#' @importFrom   dplyr left_join select
#' @importFrom   magrittr %>% %$%
#' @export
get_dependency <- function(annotation, get_token = FALSE) {
  dep <- annotation$dependency

  # silence R CMD check warnings
  id <- sid <- tid <- word <- lemma <- NULL

  if (get_token) {
    dep <- dplyr::left_join(dep, dplyr::select(annotation$token, id, sid, tid, word, lemma))
    dep <- dplyr::left_join(dep, dplyr::select(annotation$token, id, sid_target = sid, tid_target = tid,
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
#'  The returned data frame includes the following columns:
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
#' get_coreference(obama) %$%
#'   table(gender, id)
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
#'  \item{"entity_type"}{ - character. When using default models, one of "LOCATION", "PERSON", "ORGANIZATION", "MONEY", "PERCENT", "DATE", "TIME".}
#'  \item{"entity"}{ - character. Raw words of the named entity in the text.}
#'  \item{"entity_normalized"}{ - character. Normalized version of the entity.}
#' }
#'
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
#' get_entity(obama) %>%
#'  use_series(entity_type) %>%
#'  table()
#'
#' # what are the most common locations mentioned?
#' get_entity(obama) %>%
#'   filter(entity_type == "LOCATION") %>%
#'   use_series(entity) %>%
#'   table() %>%
#'   sort(decreasing = TRUE) %>%
#'   head(n = 25)
#'
#' # what are the most common organizations mentioned?
#' get_entity(obama) %>%
#'   filter(entity_type == "ORGANIZATION") %>%
#'   use_series(entity) %>%
#'   table() %>%
#'   sort(decreasing = TRUE) %>%
#'   head(n = 25)
#'
#' @export
get_entity <- function(annotation) {
  annotation$entity
}

#' Access sentiment scores from an annotation object
#'
#' Sentiment analysis attempts to extract the attitudes of the narrator or speaker
#' towards their object of study. This function extracts a sentiment score from 0
#' to 4 from each sentence in the annotation.
#'
#' @param annotation   an annotation object
#'
#' @return
#'  Returns an object of class \code{c("tbl_df", "tbl", "data.frame")}
#'  containing one row for every sentence in the corpus.
#'
#'  The returned data frame includes the following columns:
#'
#' \itemize{
#'  \item{"id"}{ - integer. Id of the source document.}
#'  \item{"sid"}{ - integer. Sentence id.}
#'  \item{"pred_class"}{ - integer. Predicted sentiment class of the sentence, from 0 (worst) to 4 (best).}
#'  \item{"p0"}{ - double. Predicted probability of the sentence being class 0.}
#'  \item{"p1"}{ - double. Predicted probability of the sentence being class 1.}
#'  \item{"p2"}{ - double. Predicted probability of the sentence being class 2.}
#'  \item{"p3"}{ - double. Predicted probability of the sentence being class 3.}
#'  \item{"p4"}{ - double. Predicted probability of the sentence being class 4.}
#' }
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
#' get_sentiment(obama) %>%
#'   group_by(id) %>%
#'   summarize(mean(pred_class), se = sd(pred_class) / sqrt(n()))
#'
#' @export
get_sentiment <- function(annotation) {
  annotation$sentiment
}

#' Access triples from an annotation object
#'
#' Relationship triples contain a subject, object, and relationship, all
#' extracted directly from the text. They represent factual statements whereby
#' the subject and object are related by the words consituting the relation.
#'
#' @param annotation   an annotation object
#'
#' @return
#'  Returns an object of class \code{c("tbl_df", "tbl", "data.frame")}
#'  containing one row for every triple found in the corpus.
#'
#'  The returned data frame includes the following columns:
#'
#' \itemize{
#'  \item{"id"}{ - integer. Id of the source document.}
#'  \item{"subject"}{ - character. Raw text of the triple's subject.}
#'  \item{"object"}{ - character. Raw text of the triple's object.}
#'  \item{"relation"}{ - character. Raw text of the triple's relation.}
#'  \item{"confidence"}{ - double. Confidence score from 0 (least confident) to 1 (most confident).}
#'  \item{"be_prefix"}{ - integer. Equals 1 if the triple's relationship has a form of "to be" as a prefix.}
#'  \item{"be_suffix"}{ - integer. Equals 1 if the triple's relationship has a form of "to be" as a suffix.}
#'  \item{"of_suffix"}{ - integer. Equals 1 if the triple's relationship has a form of "of" as a suffix.}
#'  \item{"tmod"}{ - integer. Equals 1 if the triple is a temporal modifier.}
#'  \item{"sid"}{ - integer. Sentence id of the triple.}
#'  \item{"tid_subject"}{ - integer. Token id at the start of the triple's subject.}
#'  \item{"tid_subject_end"}{ - integer. Token id at the end of the triple's subject.}
#'  \item{"tid_object"}{ - integer. Token id at the start of the triple's object.}
#'  \item{"tid_object_end"}{ - integer. Token id at the end of the triple's object.}
#'  \item{"tid_relation"}{ - integer. Token id at the start of the triple's relation.}
#'  \item{"tid_relation_end"}{ - integer. Token id at the end of the triple's relation.}
#' }
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @references
#'
#'   Manning, Christopher D., Mihai Surdeanu, John Bauer, Jenny Finkel, Steven J. Bethard, and
#'   David McClosky. 2014. \href{http://nlp.stanford.edu/pubs/StanfordCoreNlp2014.pdf}{The Stanford CoreNLP Natural Language Processing Toolkit}.
#'   In: \emph{Proceedings of the 52nd Annual Meeting of the Association for Computational Linguistics: System Demonstrations, pp. 55-60.}
#'
#'    Gabor Angeli, Melvin Johnson Premkumar, and Christopher D. Manning. Leveraging Linguistic Structure For Open Domain Information Extraction.
#'    In: \emph{Proceedings of the Association of Computational Linguistics (ACL), 2015}.
#'
#' @examples
#'
#' # what are the most common relations in the text?
#' get_triple(obama) %>%
#'   use_series(relation) %>%
#'   table() %>%
#'   sort(decreasing = TRUE) %>%
#'   head(n = 40L)
#'
#' @export
get_triple <- function(annotation) {
  annotation$triple
}
