% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-dtm.R
\name{vocab_builder}
\alias{vocab_builder}
\title{A fast unigram vocabulary builder}
\usage{
vocab_builder(data, text)
}
\arguments{
\item{data}{Data.frame with one column of texts}

\item{text}{Name of the column with documents' text}
}
\value{
returns a list of unique terms in a corpus
}
\description{
A streamlined function to take raw texts from a column of a data.frame and
produce a list of all the unique tokens. Tokenizes by the fixed,
single whitespace, and then extracts the unique tokens. This can be used as
input to \code{dtm_builder()} to standardize the vocabulary (i.e. the columns)
across multiple DTMs. Prior to building the vocabulary, texts should have
whitespace trimmed, if desired, punctuation removed and terms lowercased.
}
\author{
Dustin Stoltz
}
