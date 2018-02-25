## cleanNLP: A Tidy Data Model for Natural Language Processing

**Author:** Taylor B. Arnold<br/>
**License:** [LGPL-2](https://opensource.org/licenses/LGPL-2.1)

[![CRAN Version](http://www.r-pkg.org/badges/version/cleanNLP)](https://CRAN.R-project.org/package=cleanNLP)  [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/statsmaths/cleanNLP?branch=master&svg=true)](https://ci.appveyor.com/project/statsmaths/cleanNLP) [![Travis-CI Build Status](https://travis-ci.org/statsmaths/cleanNLP.svg?branch=master)](https://travis-ci.org/statsmaths/cleanNLP) [![Coverage Status](https://img.shields.io/codecov/c/github/statsmaths/cleanNLP/master.svg)](https://codecov.io/github/statsmaths/cleanNLP?branch=master) ![Downloads](http://cranlogs.r-pkg.org/badges/cleanNLP)

## Overview

The **cleanNLP** package is designed to make it as painless as possible
to turn raw text into feature-rich data frames. You can download the
package from within R, either directly from CRAN:
```{r}
install.packages("cleanNLP")
```
Or you may grab the development version using devtools:
```{r}
devtools::install_github("statsmaths/cleanNLP")
```
As described in detail below, the package offers four backends that
can be used for parsing text:

- **tokenizers**: a fast parser that only requires the stringi package,
but produces only tokenized text
- **udpipe**: a parser with no external dependencies that produces
tokens, lemmas, part of speech tags, and dependency relationships. The
recommended starting point given its balance between ease of use and
functionality. It also supports the widest range of natural languages.
- **spacy**: based on the Python library, a more feature complete parser
that included named entity recognition and word embeddings. It does require
a working Python installation and some other set-up. Recommended for users
who are familiar with Python or plan to make heavy use of the package.
- **corenlp**: based on the Java library with the same name. Supports
coreferences and other bleeding-edge annotation tasks. Not recommended
for most users given its slow speed and difficult set-up.

Many function names have changed in the 2.0 release of the package, so
please look at this file and the documentation for details.

## Basic usage

We take as an example the opening lines of Douglas Adam's
*Life, the Universe and Everything*:

```{r}
text <- c("The regular early morning yell of horror was the sound of",
          "Arthur Dent waking up and suddenly remembering where he",
          "was. It wasn't just that the cave was cold, it wasn't just",
          "that it was damp and smelly. It was the fact that the cave",
          "was in the middle of Islington and there wasn't a bus due",
          "for two million years.")
text <- paste(text, collapse = " ")
```

A minimal working example of using **cleanNLP** consists of loading the
package, setting up the NLP backend, initializing the backend, and running
the function `cnlp_annotate`. Because our input is a text string we set
`as_strings` to `TRUE` (the default is to assume that we are giving the
function paths to where the input data sits on the local machine"):

```{r}
library(cleanNLP)
cnlp_init_udpipe()
obj <- cnlp_annotate(text, as_strings = TRUE)
```

Here, we used the udpipe backend, which is the recommended place to start.
The returned annotation
object is nothing more than a list of data frames (and one matrix),
similar to a set of tables within a database. The names of these tables
are:

```{r}
names(obj)
```
```
## [1] "coreference" "dependency"  "document"    "entity"      "sentence"
## [6] "token"       "vector"
```

The canonical way of accessing these data frames is by using functions of
the form `cnlp_get_TABLE`. For example, the document table gives metadata about
each document in the corpus, which here consists of only a single
document:

```{r}
cnlp_get_document(obj)
```
```
      id                time version language  uri
1   doc1 2018-01-02 16:37:43   0.2.2  english <NA>
```

The tokens table has one row for each word in the input text, giving data
about each word such as its lemmatized form and its part of speech. We
access these table with the `get_token` function:

```{r}
head(cnlp_get_token(obj))
```
```
> head(cnlp_get_token(obj))
      id sid tid    word   lemma upos pos cid pid case definite degree gender
1   doc1   1   1     The     the  DET  DT   0   1 <NA>      Def   <NA>   <NA>
2   doc1   1   2 regular regular  ADJ  JJ   4   1 <NA>     <NA>    Pos   <NA>
3   doc1   1   3   early   early  ADJ  JJ  12   1 <NA>     <NA>    Pos   <NA>
4   doc1   1   4 morning morning NOUN  NN  18   1 <NA>     <NA>   <NA>   <NA>
5   doc1   1   5    yell    yell NOUN  NN  26   1 <NA>     <NA>   <NA>   <NA>
6   doc1   1   6      of      of  ADP  IN  31   1 <NA>     <NA>   <NA>   <NA>
  mood num_type number person pron_type tense verb_form
1 <NA>     <NA>   <NA>   <NA>       Art  <NA>      <NA>
2 <NA>     <NA>   <NA>   <NA>      <NA>  <NA>      <NA>
3 <NA>     <NA>   <NA>   <NA>      <NA>  <NA>      <NA>
4 <NA>     <NA>   Sing   <NA>      <NA>  <NA>      <NA>
5 <NA>     <NA>   Sing   <NA>      <NA>  <NA>      <NA>
6 <NA>     <NA>   <NA>   <NA>      <NA>  <NA>      <NA>
```

The output from the `cnlp_get` functions are (mostly) pre-calculated.
All of the hard work is done in the `cnlp_annotate` function.

## Text Interchange Format (TIF)

The **cleanNLP** package supports the text-interchange formats (see:
[tif](https://github.com/ropensci/tif)), agreed formats for the input
and output of textual data developed at the 2017 rOpenSci Text Workshop
hosted at LSE.

To use the text interchange format as an input, first construct the
input data as a data frame with the first column containing document ids,
the second the raw text, and other columns containing metadata:

```{r}
text <- c("It is better to be looked over than overlooked.",
         "Real stupidity beats artificial intelligence every time.",
         "The secret of getting ahead is getting started.")
tif_input <- data.frame(id = c("West", "Pratchett", "Twain"),
                        text = text,
                        full_name = c("Mae West",
                                      "Terry Pratchett",
                                      "Mark Twain"),
                        birthplace = c("Brooklyn, New York",
                                       "Florida, Missouri",
                                       "Beaconsfield, Buckinghamshire, England"),
                        stringsAsFactors = FALSE)
```

And then run the function `cnlp_annotate` on in the input:

```{r}
library(cleanNLP)
cnlp_init_udpipe()
obj <- cnlp_annotate(tif_input)
```

The object `obj` is a cleanNLP list of tables. To get the tif output
format, which is a good starting format for working with the annotated
data, run `cnlp_get_tif`:

```{r}
head(cnlp_get_tif(obj))
```
```
# A tibble: 6 x 25
     id   sid   tid   word  lemma  upos   pos   cid   pid  case definite degree
  <chr> <int> <int>  <chr>  <chr> <chr> <chr> <dbl> <int> <chr>    <chr>  <chr>
1  West     1     1     It     it  PRON   PRP     0     1   Nom     <NA>   <NA>
2  West     1     2     is     be   AUX   VBZ     3     1  <NA>     <NA>   <NA>
3  West     1     3 better better   ADJ   JJR     6     1  <NA>     <NA>    Cmp
4  West     1     4     to     to  PART    TO    13     1  <NA>     <NA>   <NA>
5  West     1     5     be     be   AUX    VB    16     1  <NA>     <NA>   <NA>
6  West     1     6 looked   look  VERB   VBN    19     1  <NA>     <NA>   <NA>
# ... with 13 more variables: gender <chr>, mood <chr>, number <chr>,
#   person <chr>, pron_type <chr>, tense <chr>, verb_form <chr>, voice <chr>,
#   source <int>, relation <chr>, word_source <chr>, lemma_source <chr>,
#   spaces <dbl>
```

The output is now a single data frame that can be saved to disk or
used in models and plots. When possible, we recommend using the tif
input format for annotating text.

## Backends

Installation details for the spacy and corenlp backends, both of which
required external dependencies, are given below.

### spacy backend (python)

To use the Python based backend spacy, users must first install an up to
date version of Python. For users without prior experience, we recommend
installing the Python 3.6 version of
[Anaconda Python](https://www.continuum.io/downloads). Anaconda has both
command line and GUI installers for all major platforms; these include
many of the basic scientific Python modules (these can be troublesome
to install otherwise). Make sure that you have at least version 3.6.1
or greater. Of particular note, we do not recommend or support
using the default system installation of Python that is bundled with
MacOS, Ubuntu, and many other linux distributions. This version is often
out-of-date, almost impossible to update, and causes permissions issues
when installing modules.

Once Python is installed, the next step is to install the spacy module
and its associated models. There are detailed instructions for doing
this on the [spacy website](https://spacy.io/docs/usage/) that support
many different platforms and versions of Python. Note that on windows
you must also have a working version of Virtual Studio Express
(preferably the 2015 version). If you installed the
most recent version of Anaconda Python 3.6 as directed above, simply
run the following commands in the terminal (Mac/Linux) or shell (Windows):

```
conda config --add channels conda-forge
conda install spacy
python -m spacy download en
```

The last line downloads the English models ("en"); repeat or replace
with French ("fr") or German ("de") based on your needs.

Finally, the R package **reticulate** must also be installed, which
can be downloaded from CRAN:
```{r}
install.packages("reticulate")
```

And, before running any cleanNLP command, run `use_python` to set the
location of the Python executable. If using Anaconda Python on a Mac
with the default settings, for example, the following should be correct:

```{r}
use_python("/Users/USER/anaconda3/bin/python")
```

Generally, we have found that the only difficult step in this process
is installing spacy. Getting Python is almost always issue free as well
as install the **reticulate** package. On a Mac, we've found that best
solution is to start with a fresh version of Anaconda 3.6 (the 2.7
series causes problem even though it technically should work). We have
not had an issue getting the library installed when doing this.
On Windows, a fresh version of Anaconda also helps, but even then there
can be further issues. On Windows, it seems that there is no
general solution that works for everyone, unfortunately. Your best bet
is to follow the instructions above and then look up your particular
warning on the [spacy issues page](https://github.com/explosion/spacy/issues).

After the backend is install, you should be able to run the code in the
preceding section "Basic usage" as given.

### corenlp backend (Java)

In order to make use of the Java-based corenlp backend, a version of
Java >= 7.0 must be installed and the **rJava** package must be set up.
This should be straightforward, and generally runs without issue on
Linux and Windows. On Mac, there are issues arising from conflicts with
the system version of Java. The detailed instructions
[Problem With rJava On Mac OS X El Capitan](http://charlotte-ngs.github.io/2016/01/MacOsXrJavaProblem.html) from
from Peter von Rohr have solved these issues on all of our test systems.
For additional help, see the GitHub issues tracker and submit any new
bugs that arise.

Once these system requirements are met, we can install the ".jar" files
inside of R with the following function:

```{r}
cnlp_download_corenlp()
```

These files are large and may take several minutes to download. The Java
backend is then configured with `cnlp_init_corenlp`. The easiest
interface is to specify an annotation level from 0 to 3, with higher numbers
including more models but taking increasingly long to parse the text.
Setting it equal to 2 is a good balance between time and
feature-richness:

```{r}
cnlp_init_corenlp(anno_level = 2L, lib_location = lib_loc)
```

After the pipeline is loaded, we again call run_annotators and set the
backend to "coreNLP" (by default run_annotators will use whichever backend
for most recently initialized, so this option is technically not
needed if you just ran `cnlp_init_corenlp`):

```{r}
obj <- cnlp_annotate(text, as_strings = TRUE, backend = "coreNLP")
obj
```
```
##
## A CleanNLP Annotation:
##   num. documents: 1
```

The annotation object contains the same tables as the spaCy models,
with slightly different fields filled in.

## More information

This document is meant just to get users up to speed enough to starting being
useful with the **cleanNLP** package. There are many more options and helper
functions available to make working with textual data as easy as working with
tabular datasets. For a full example of using the package to do an analysis
of a corpus see the vignette:

 - [Exploring the State of the Union Addresses: A Case Study with cleanNLP](https://cran.r-project.org/package=cleanNLP/vignettes/case_study.html).

For more detailed information about the fields in each of the tables, users
should consult the paper:

> 'A Tidy Data Model for Natural Language Processing Using cleanNLP.' Taylor Arnold. The R Journal, 9.2, 1-20 (2017).

## Note

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.


