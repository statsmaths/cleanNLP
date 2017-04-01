## Installation

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
As described in detail below, the package has a bare-bones parser that
does not require any additional system dependencies. However, to benefit
from the package most users will want to load either a Python or Java
backend. The Python backend uses the spaCy module, which can be installed
by running the following in a terminal:
```{sh}
pip install -U spacy
python -m spacy download en
```
Further instructions are available at [spacy.io](https://spacy.io/docs/usage/models).

## Basic usage - tokenizers

We will take the opening lines of Douglas Adam's
*Life, the Universe and Everything* as a small example corpus for
exploring the **cleanNLP** package:

```{r}
text <- c("The regular early morning yell of horror was the sound of",
          "Arthur Dent waking up and suddenly remembering where he",
          "was. It wasn't just that the cave was cold, it wasn't just",
          "that it was damp and smelly. It was the fact that the cave",
          "was in the middle of Islington and there wasn't a bus due",
          "for two million years.")
text <- paste(text, collapse = " ")
```

A minimal working example of using **cleanNLP** consists of simply loading
the package and calling the `annotate` function. Because our input is
a text string we set `as_strings` to `TRUE` (the default is to assume that
we are giving the function paths to where the input data sits on the
local machine"):

```{r}
library(cleanNLP)
obj <- annotate(text, as_strings = TRUE)
obj
```
```
##
## A CleanNLP Annotation:
##   num. documents: 1
```

The returned annotation object is nothing more than a list of data frames,
similar to a set of tables within a database. The canonical way of accessing
these data frames is by using functions of the form "get_*". For example, one
table gives metadata about each document in the "corpus", which here consists
of only one document:

```{r}
get_document(obj)
```
```
## # A tibble: 1 × 5
##      id                time version language
##   <int>              <dttm>   <chr>    <chr>
## 1     0 2017-04-01 17:54:28     1.2      n/a
## # ... with 1 more variables: uri <chr>
```

The tokens table has one row for each word in the input text, giving data
about each word such as its lemmatized form and its part of speech. We
access these table with the `get_token` function:

```{r}
get_token(obj)
```
```
## # A tibble: 64 × 11
##       id   sid   tid    word lemma  upos   pos speaker  wiki   cid
##    <int> <int> <int>   <chr> <chr> <chr> <chr>   <chr> <chr> <int>
## 1      0     1     0    ROOT  <NA>  <NA>  <NA>    <NA>  <NA>    NA
## 2      0     1     1     The  <NA>  <NA>  <NA>    <NA>  <NA>    NA
## 3      0     1     2 regular  <NA>  <NA>  <NA>    <NA>  <NA>    NA
## 4      0     1     3   early  <NA>  <NA>  <NA>    <NA>  <NA>    NA
## 5      0     1     4 morning  <NA>  <NA>  <NA>    <NA>  <NA>    NA
## 6      0     1     5    yell  <NA>  <NA>  <NA>    <NA>  <NA>    NA
## 7      0     1     6      of  <NA>  <NA>  <NA>    <NA>  <NA>    NA
## 8      0     1     7  horror  <NA>  <NA>  <NA>    <NA>  <NA>    NA
## 9      0     1     8     was  <NA>  <NA>  <NA>    <NA>  <NA>    NA
## 10     0     1     9     the  <NA>  <NA>  <NA>    <NA>  <NA>    NA
## # ... with 54 more rows, and 1 more variables: cid_end <int>
```

The output from the "get_*" tables are (mostly) pre-calculated. All of the hard
work is done in the `annotate` function. Notice that most of the features in the
tokens table are filled in. Creating these requires further set-up, which we will
discuss next.

There are three "backends" for parsing text in **cleanNLP**. These are:

- an R-based version using only the **tokenizers** package. It offers minimal
output but requires no external dependencies.
- a Python-based parser using the [spaCy](https://spacy.io/) library.
Requires installing Python and the library separately; this is generally
pain free and works with any version of Python >= 2.7. The library is very
fast and provides the basic annotators for parsing text such as lemmatization,
part of speech tagging, dependency parsing, and named entity recognition.
There is also support for computing word embeddings in English.
- a Java-based parser using the [CoreNLP](http://stanfordnlp.github.io/CoreNLP/)
library. Setting this up is often more involved and the data files are quite
large. The pipeline also takes significantly longer than the spaCy implementation.
However, the CoreNLP parser offers many more bleeding-edge annotation tasks
such as coreference resolution and speaker detection.

With no further set-up, the package defaults to the R-based version, as used
above. In this case only the tokens and document tables will be non-empty and
many of the columns in the tokens table will be all `NA`s. The only benefit of
the R-based version is its lack of external dependencies. We supply it mainly
for testing and teaching purposes when access to the machine(s) running the
code are not under our control. For most use-cases we recommend using the spaCy
library as it strikes a balance between features, speed, and ease of set-up.
The Java backend should be used when access to the advanced annotation tasks
is required.

## Python backend - spaCy

To use the Python backend with spaCy, we first need to install Python and spaCy
on our system. The package **reticulte** must also be installed; see the
[spaCy website](https://spacy.io/docs/usage/) for download instructions on
your platform. After this is done, we can run:

```{r}
library(cleanNLP)
init_clean_nlp("spaCy")
```
```
##
## A CleanNLP Annotation:
##   num. documents: 1
```

Subsequent calls to `annotate` will by default use the Python backend (there
is an option to explicitly specify which backend to make use of).

```{r}
obj <- annotate(text, as_strings = TRUE)
obj
```
```
##
## A CleanNLP Annotation:
##   num. documents: 1
```

You should notice that the output object looks very similar. Extracting off
the tokens table shows the same column names and types but with more information
populated:

```{r}
get_token(obj)
```
```
## # A tibble: 71 × 11
##       id   sid   tid    word   lemma  upos   pos speaker  wiki   cid
##    <int> <int> <int>   <chr>   <chr> <chr> <chr>   <chr> <chr> <int>
## 1      0     0     0    ROOT    ROOT  <NA>  <NA>    <NA>  <NA>    NA
## 2      0     0     1     The     the   DET    DT    <NA>  <NA>     0
## 3      0     0     2 regular regular   ADJ    JJ    <NA>  <NA>     4
## 4      0     0     3   early   early   ADJ    JJ    <NA>  <NA>    12
## 5      0     0     4 morning morning  NOUN    NN    <NA>  <NA>    18
## 6      0     0     5    yell    yell  NOUN    NN    <NA>  <NA>    26
## 7      0     0     6      of      of   ADP    IN    <NA>  <NA>    31
## 8      0     0     7  horror  horror  NOUN    NN    <NA>  <NA>    34
## 9      0     0     8     was      be  VERB   VBD    <NA>  <NA>    41
## 10     0     0     9     the     the   DET    DT    <NA>  <NA>    45
## # ... with 61 more rows, and 1 more variables: cid_end <int>
```

We now have added lemmas, part of speech tags, and character offsets into the
original text. Two additional tables have also been populated, the set of
dependencies:

```{r}
get_dependency(obj)
```
```
## # A tibble: 68 × 7
##       id   sid   tid sid_target tid_target relation relation_full
##    <int> <int> <int>      <int>      <int>    <chr>         <chr>
## 1      0     0     1          0          5      det          <NA>
## 2      0     0     2          0          5     amod          <NA>
## 3      0     0     3          0          5     amod          <NA>
## 4      0     0     4          0          5 compound          <NA>
## 5      0     0     5          0          8    nsubj          <NA>
## 6      0     0     6          0          5     prep          <NA>
## 7      0     0     7          0          6     pobj          <NA>
## 8      0     0     8          0          0     ROOT          <NA>
## 9      0     0     9          0         10      det          <NA>
## 10     0     0    10          0          8     attr          <NA>
## # ... with 58 more rows
```

And a dataset of named entities:

```{r}
get_entity(obj)
```
```
## # A tibble: 8 × 7
##      id   sid   tid tid_end entity_type entity entity_normalized
##   <int> <int> <int>   <int>       <chr>  <chr>             <chr>
## 1     0     0     2       3        TIME   <NA>              <NA>
## 2     0     0     3       1        TIME   <NA>              <NA>
## 3     0     0    11       3      PERSON   <NA>              <NA>
## 4     0     0    12       1      PERSON   <NA>              <NA>
## 5     0     2    12       3         GPE   <NA>              <NA>
## 6     0     2    21       3        DATE   <NA>              <NA>
## 7     0     2    22       1        DATE   <NA>              <NA>
## 8     0     2    23       1        DATE   <NA>              <NA>
```

The only table created by the Python backend are word embeddings. These are
quite large and so we do not produce them by default. To change the settings
of which annotation tasks are used we first call `set_spacy_properties` and
then reinitalize the cleanNLP pipeline

```{r}
set_spacy_properties(vector_flag = TRUE)
init_clean_nlp("spaCy")
obj <- annotate(text, as_strings = TRUE)
```

Unlike the other elements of `obj`, the word embeddings are stored as a matrix
for efficiency. The first two rows give links into the main tokens table and
the next 300 columns give the actual embedding:

```{r}
vec <- get_vector(obj)
dim(vec)
vec[1:10,1:9]
```
```
##       [,1] [,2] [,3]    [,4]    [,5]    [,6]    [,7]    [,8]    [,9]
##  [1,]    0    0    0  0.2720 -0.0620 -0.1884  0.0232 -0.0182  0.0067
##  [2,]    0    0    1 -0.0361 -0.3121 -0.2657  0.4840 -0.2206  0.2130
##  [3,]    0    0    2  0.1277  0.4792 -0.0374 -0.1151 -0.0470 -0.0189
##  [4,]    0    0    3  0.2636  0.5578 -0.1662  0.3180 -0.3656 -0.0469
##  [5,]    0    0    4 -0.4706 -0.0855 -0.6264 -0.2060  0.7103  0.1252
##  [6,]    0    0    5  0.0602  0.2180 -0.0425 -0.3862 -0.1539  0.0346
##  [7,]    0    0    6  0.1039 -0.3952  0.2193 -0.2240  0.6035  0.1498
##  [8,]    0    0    7 -0.0441  0.3661  0.1803 -0.2494 -0.0981  0.0333
##  [9,]    0    0    8  0.2720 -0.0620 -0.1884  0.0232 -0.0182  0.0067
## [10,]    0    0    9  0.1034  0.6273 -0.2118 -0.2848  0.3848  0.1572
```

Alternatively, the `set_spacy_properties` can also be used to turn off the
tagging of named entities.

## Java backend - coreNLP

In order to make use of the Java backend, a version of Java >= 7.0 must be
installed and the **rJava** package must be set up. This should be
straightforward, but this has caused problems on some machines, particularly
with macOS. For help, see the GitHub issues tracker. Once these system
requirements are met, we can install the ".jar" files with

```{r}
download_core_nlp()
```

These files are large and may take several minutes to download. The Java
backend is then configured with `set_java_properties`. The easiest
interface is to specify a speed code from 0 to 8, with higher numbers
including more models but taking increasingly long to parse the text.
Setting it equal to 2 is a good reasonable balance between time and
feature-richness:

```{r}
set_java_properties(speed = 2L)
```

We once again call `init_clean_nlp`, this time specifying that we want
the "java" version to by initialized:

```{r}
init_clean_nlp(type = "java", )
```
```{r}
lib_loc <- "~/local/core_nlp_files/stanford-corenlp-full-2016-10-31"
init_clean_nlp(type = "coreNLP", lib_location = lib_loc)
```

After the pipeline is loaded, we again call annotate and set the
backend to "coreNLP":

```{r}
obj <- annotate(text, as_strings = TRUE, backend = "coreNLP")
obj
```
```
##
## A CleanNLP Annotation:
##   num. documents: 1
```

The annotation object contains the same tables as the other models,
with slightly different fields filled in.

## Saving annotations

One an annotation object is created there are two ways of saving the output.
Using `saveRDS` saves the output as a binary file which can be read back into
R at any time. Alternatively, the function `write_annotation` saves the annotation
as a collection of comma separated files:

```{r}
od <- tempfile()
write_annotation(obj, od)
dir(od)
```

These may be read back into R as an annotation object using `read_annotation`:

```{r}
anno <- read_annotation(od)
```

Alternatively, as these are just comma separated values, the data may be read
directly using `read.csv` in R or whatever other programming language or software
a user would like to work with.

## More information

This document is meant just to get users up to speed enough to starting being
useful with the **coreNLP** package. There are many more options and helper
functions available to make working with textual data as easy as working with
tabular datasets. For a full example of using the package to do an analysis
of a corpus see the vignette: *Exploring the State of the Union Addresses: A Case Study with cleanNLP*.

For more detailed information about the fields in each of the tables, users
can consult the help pages for the "get" functions. An even more detailed
guide to the fields and the underlying philosophy is given in the vignette:
*A Data Model for the NLP Pipeline*.
