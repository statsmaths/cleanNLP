## cleanNLP: A Tidy Data Model for Natural Language Processing

**Author:** Taylor B. Arnold<br/>
**License:** [LGPL-2](https://opensource.org/licenses/LGPL-2.1)

[![CRAN Version](http://www.r-pkg.org/badges/version/cleanNLP)](https://CRAN.R-project.org/package=cleanNLP) [![Travis-CI Build Status](https://travis-ci.org/statsmaths/cleanNLP.svg?branch=master)](https://travis-ci.org/statsmaths/cleanNLP) ![Downloads](http://cranlogs.r-pkg.org/badges/cleanNLP)

## Overview

The **cleanNLP** package is designed to make it as painless as possible
to turn raw text into feature-rich data frames. You can download the
package from within R directly from CRAN:
```{r}
install.packages("cleanNLP")
```
There are four backends available to parse text, each with their own pros and
cons. They are:

- **stringi**: a fast parser that only requires the stringi package,
but produces only tokenized text
- **udpipe**: a parser with no external dependencies that produces
tokens, lemmas, part of speech tags, and dependency relationships. The
recommended starting point given its balance between ease of use and
functionality. It also supports the widest range of natural languages.
- **spacy**: based on the Python library, a more feature complete parser
that included named entity recognition and word embeddings. It does require
a working Python installation and some other set-up. Recommended for users
who are familiar with Python or plan to make heavy use of the package.
- **corenlp**: another Python library (formally Java) that is an official
port of the Java library of the same name.

In order to us the two Python backends, you must install the associated
cleanNLP python module. We recommend and support the Python 3.7 version of
[Anaconda Python](https://www.anaconda.com/distribution/#download-section).
After obtaining Python, install the module by running pip in a terminal:

```{py}
pip install cleannlp
```

Many function names have changed in the 3.0 release of the package, so
please look at this file and the documentation for details.

## Basic usage

In order to tokenize text, you need to organize your data as a data frame.
The raw text to parse should be in a column called "text"; you can optionally
provide a column named "id" that gives a unique identifier to each document.
Finally, if the data frame contains a column called "path" (and no "text"
column), the function will attempt to load text files from the path's prior to
running the annotations.

We take as an example the opening lines of Douglas Adam's
*Life, the Universe and Everything*.

```{r}
text <- c("The regular early morning yell of horror was the sound of",
          "Arthur Dent waking up and suddenly remembering where he",
          "was. It wasn't just that the cave was cold, it wasn't just",
          "that it was damp and smelly. It was the fact that the cave",
          "was in the middle of Islington and there wasn't a bus due",
          "for two million years.")
input <- data.frame(text=paste(text, collapse = " "), stringsAsFactors=TRUE)
```

A minimal working example of using **cleanNLP** consists of loading the
package, setting up the NLP backend, initializing the backend, and running
the function `cnlp_annotate`. We will use the udpipe

```{r}
library(cleanNLP)
cnlp_init_udpipe()
obj <- cnlp_annotate(input)
```

Here, we used the udpipe backend, which is the recommended place to start.
The returned annotation object is nothing more than a list of data frames
similar to a set of tables within a database. If we load the **tibble** package,
which is recommended but optional, these data frames will print out in a
compact way:

```{r}
library(tibble)
obj
```
```
> obj
$token
# A tibble: 68 x 10
      id   sid   tid token  lemma upos  xpos  feats          tid_source relation
 * <int> <int> <dbl> <chr>  <chr> <chr> <chr> <chr>          <chr>      <chr>
 1     1     1     1 The    the   DET   DT    Definite=Def|… 5          det
 2     1     1     2 regul… regu… ADJ   JJ    Degree=Pos     5          amod
 3     1     1     3 early  early ADJ   JJ    Degree=Pos     4          amod
 4     1     1     4 morni… morn… NOUN  NN    Number=Sing    5          compound
 5     1     1     5 yell   yell  NOUN  NN    Number=Sing    10         nsubj
 6     1     1     6 of     of    ADP   IN    NA             7          case
 7     1     1     7 horror horr… NOUN  NN    Number=Sing    5          nmod
 8     1     1     8 was    be    AUX   VBD   Mood=Ind|Numb… 10         cop
 9     1     1     9 the    the   DET   DT    Definite=Def|… 10         det
10     1     1    10 sound  sound NOUN  NN    Number=Sing    0          root
# … with 58 more rows

$document

1 The regular early morning yell of horror was the sound of Arthur Dent waking
up and suddenly remembering where he was. It wasn't just that the cave was cold,
it wasn't just that it was damp and smelly. It was the fact that the cave was
in the middle of Islington and there wasn't a bus due for two million years.
  id
1  1
```

There are just two tables here. A document table gives metadata about
each document in the corpus, which here consists of only a single
document. The tokens table has one row for each word in the input text,
giving data about each word such as its lemmatized form and its part of speech:

```{r}
head(obj$token)
```
```
# A tibble: 6 x 10
     id   sid   tid token  lemma  upos  xpos  feats          tid_source relation
  <int> <int> <dbl> <chr>  <chr>  <chr> <chr> <chr>          <chr>      <chr>
1     1     1     1 The    the    DET   DT    Definite=Def|… 5          det
2     1     1     2 regul… regul… ADJ   JJ    Degree=Pos     5          amod
3     1     1     3 early  early  ADJ   JJ    Degree=Pos     4          amod
4     1     1     4 morni… morni… NOUN  NN    Number=Sing    5          compound
5     1     1     5 yell   yell   NOUN  NN    Number=Sing    10         nsubj
6     1     1     6 of     of     ADP   IN    NA             7          case
```


## Note

Please note that this project is released with a
[Contributor Code of Conduct](CONDUCT.md). By participating in this project
you agree to abide by its terms.
