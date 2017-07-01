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




