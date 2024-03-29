% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-dtm.R
\name{dtm_builder}
\alias{dtm_builder}
\title{A fast unigram DTM builder}
\usage{
dtm_builder(
  data,
  text,
  doc_id = NULL,
  vocab = NULL,
  chunk = NULL,
  dense = FALSE,
  omit_empty = FALSE
)
}
\arguments{
\item{data}{Data.frame with column of texts and column of document ids}

\item{text}{Name of the column with documents' text}

\item{doc_id}{Name of the column with documents' unique ids.}

\item{vocab}{Default is \code{NULL}, if a list of terms is provided, the function
will return a DTM with terms restricted to this vocabulary.
Columns will also be in the same order as the list of terms.}

\item{chunk}{Default is \code{NULL}, if an integer is provided, the function will
"re-chunk" the corpus into new documents of a particular length.
For example, \code{100L} will divide the corpus into new documents
with 100 terms (with the final document likely including
slightly less than 100).}

\item{dense}{The default (\code{FALSE}) is to return a matrix of class
"dgCMatrix" as DTMs typically have
mostly zero cells. This is much more memory efficient.
Setting dense to \code{TRUE} will return a normal base \code{R} matrix.}

\item{omit_empty}{Logical (default = \code{FALSE}) indicating whether to omit rows
that are empty after stopping any terms.}
}
\value{
returns a document-term matrix of class "dgCMatrix"
or class "matrix"
}
\description{
A streamlined function to take raw texts from a column of a data.frame and
produce a sparse Document-Term Matrix (of generic class "dgCMatrix").
}
\details{
The function is fast because it has few bells and whistles:
\itemize{
\item No weighting schemes other than raw counts
\item Tokenizes by the fixed, single whitespace
\item Only tokenizes unigrams. No bigrams, trigrams, etc...
\item Columns are in the order unique terms are discovered
\item No preprocessing during building
\item Outputs a basic sparse Matrix or dense matrix
}

Weighting or stopping terms can be done efficiently after the fact with
simple matrix operations, rather than achieved implicitly within the
function itself. For example, using the \code{dtm_stopper()} function.
Prior to creating the DTM, texts should have whitespace trimmed, if
desired, punctuation removed and terms lowercased.

Like \code{tidytext}'s DTM functions, \code{dtm_builder()} is optimized for use
in a pipeline, but unlike \code{tidytext}, it does not build an intermediary
tripletlist, so \code{dtm_builder()} is faster and far more memory
efficient.

The function can also \code{chunk} the corpus into documents of a given length
(default is \code{NULL}). If the integer provided is \code{200L}, this will divide
the corpus into new documents with 200 terms (with the final document
likely including slightly less than 200). If the total terms in the
corpus were less than or equal to \code{chunk} integer, this would produce
a DTM with one document (most will probably not want this).

If the vocabulary is already known, or standardizing vocabulary across
several DTMs is desired, a list of terms can be provided to the \code{vocab}
argument. Columns of the DTM will be in the order of the list of terms.
}
\examples{

library(dplyr)

my_corpus <- data.frame(
  text = c(
    "I hear babies crying I watch them grow",
    "They'll learn much more than I'll ever know",
    "And I think to myself",
    "What a wonderful world",
    "Yes I think to myself",
    "What a wonderful world"
  ),
  line_id = paste0("line", seq_len(6))
)
## some text preprocessing
my_corpus$clean_text <- tolower(gsub("'", "", my_corpus$text))

# example 1 with R 4.1 pipe
\donttest{
dtm <- my_corpus |>
  dtm_builder(clean_text, line_id)
}

# example 2 without pipe
dtm <- dtm_builder(
  data = my_corpus,
  text = clean_text,
  doc_id = line_id
)

# example 3 with dplyr pipe and mutate
\donttest{
dtm <- my_corpus \%>\%
  mutate(
    clean_text = gsub("'", "", text),
    clean_text = tolower(clean_text)
  ) \%>\%
  dtm_builder(clean_text, line_id)

# example 4 with dplyr and chunk of 3 terms
dtm <- my_corpus \%>\%
  dtm_builder(clean_text,
    line_id,
    chunk = 3L
  )
}

# example 5 with user defined vocabulary
my.vocab <- c("wonderful", "world", "haiku", "think")

dtm <- dtm_builder(
  data = my_corpus,
  text = clean_text,
  doc_id = line_id,
  vocab = my.vocab
)
}
\author{
Dustin Stoltz
}
