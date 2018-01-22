# cleanNLP 2.0

This is a major re-structuring of the cleanNLP package.
The primary changes include:

* the new udpipe backend, which gives tokenization,
POS-tags, lemmatization,and dependency parsing with no
external dependencies

* all functions return results in memory; to store data
on disk, users need to save the output manually

* use a character vector named 'id' as the document
id (it was previously an integer index); this is to
conform to the text interchange format (tif)

* functions now use the prefix 'cnlp_', following the convention
of packages such as stringi

* the cnlp_get_tfidf function now returns a named sparse
matrix in lieu of a named list

There are also many internal changes, primarily to deal
with the new spaCy (2.0) version and to make the use of
udpipe more natural.

# cleanNLP 1.10

In this version, the internal mechanisms for running
the tokenizers backend have been changed. We are now
directly calling the stringi functions with options
that better mimic those of the the spaCy and CoreNLP
backends. Despite the lack of dependency on the
tokenizers package, we will continue to use the
name "tokenizers" for the backend to maintain
backwards consistency.

As part of the change to custom stringi function, we
now also support setting the locale as part of initalizing
the tokenizers backend. This allows for an easy way of
tokenizing text where custom spaCy or coreNLP models
do not yet exist.

There is currently a pre-release version of spaCy
version 2.0.0. The current version has been tested and
runs smoothly with cleanNLP. The new neural network models
are sufficently faster and more accurate; we suggest
migrating to the version 2 series as it becomes stable
for production.

# cleanNLP 1.8.0

This version contains many internal changes to the way
that external libraries are called and referenced in
order to comply with goodpractice::gp(). Two important
user-facing changes include:

* The function `annotate` has been changed to
`run_annotations` in order to avoid a conflict
with `ggplot2::annotate`

* The function `get_token` has new options for producing
a single joined table with dependencies and entities.
This should make it easier to work with the output for
users needed more than lemmas and POS-tags but not
requiring deeper table joins.


# cleanNLP 1.7.0

This update contains several major changes, include:

* document and sentence ids now start at 1

* download function checks and warns if Java files
are already downloaded

* table joins inside of get_document() no longer
produce verbose output

* get_token() now has an option, FALSE by default,
for whether sentence ROOTS should be returned

* the speed parameter to init_coreNLP() has been
renamed as anno_level




