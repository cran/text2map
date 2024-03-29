% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-embedding-matrices.R
\name{find_transformation}
\alias{find_transformation}
\title{Find a specified matrix transformation}
\usage{
find_transformation(
  wv,
  ref = NULL,
  method = c("align", "norm", "center", "retrofit")
)
}
\arguments{
\item{wv}{Matrix of word embedding vectors (a.k.a embedding model)
with rows as terms (the source matrix to be transformed).}

\item{ref}{If \code{method = "align"}, this is the reference matrix
toward which the source matrix is to be aligned.}

\item{method}{Character vector indicating the method to use for
the transformation. Current methods include: "align",
"norm", "center", and "refrofit" -- see details.}
}
\value{
A new word embedding matrix,
transformed using the specified method.
}
\description{
Given a matrix, \eqn{B}, of word embedding vectors (source) with
terms as rows, this function finds a transformed matrix following a
specified operation. These include: centering (i.e.
translation) and normalization (i.e. scaling). In the first, \eqn{B} is
centered by subtracting column means. In the second, \eqn{B} is
normalized by the L2 norm. Both have been found to improve
word embedding representations. The function also finds a transformed
matrix that approximately aligns \eqn{B}, with another matrix,
\eqn{A}, of word embedding vectors (reference), using Procrustes
transformation (see details). Finally, given a term-co-occurrence matrix
built on a local corpus, the function can "retrofit" pretrained
embeddings to better match the local corpus.
}
\details{
Aligning a source matrix of word embedding vectors, \eqn{B}, to a
reference matrix, \eqn{A}, has primarily been used as a post-processing step
for embeddings trained on longitudinal corpora for diachronic analysis
or for cross-lingual embeddings. Aligning preserves internal (cosine)
distances, while orient the source embeddings to minimize the sum of squared
distances (and is therefore a Least Squares problem).
Alignment is accomplished with the following steps:
\itemize{
\item translation: centering by column means
\item scaling: scale (normalizes) by the L2 Norm
\item rotation/reflection: rotates and a reflects to minimize
sum of squared differences, using singular value decomposition
}

Alignment is asymmetrical, and only outputs the transformed source matrix,
\eqn{B}. Therefore, it is typically recommended to align \eqn{B} to \eqn{A},
and then \eqn{A} to \eqn{B}. However, simplying centering and norming
\eqn{A} after may be sufficient.
}
\references{
Mikel Artetxe, Gorka Labaka, and Eneko Agirre. (2018).
'A robust self-learning method for fully unsupervised
cross-lingual mappings of word embeddings.' \emph{In Proceedings
of the 56th Annual Meeting of the Association for
Computational Linguistics}. 789-798\cr
Mikel Artetxe, Gorka Labaka, and Eneko Agirre. 2019.
'An effective approach to unsupervised machine translation.'
\emph{In Proceedings of the 57th Annual Meeting of the Association
for Computational Linguistics}. 194-203\cr
Hamilton, William L., Jure Leskovec, and Dan Jurafsky. (2018).
'Diachronic Word Embeddings Reveal Statistical Laws of Semantic Change.'
\url{https://arxiv.org/abs/1605.09096v6}.\cr
Lin, Zefeng, Xiaojun Wan, and Zongming Guo. (2019).
'Learning Diachronic Word Embeddings with Iterative Stable
Information Alignment.' \emph{Natural Language Processing and
Chinese Computing}. 749-60. \doi{10.1007/978-3-030-32233-5_58}.\cr
Schlechtweg et al. (2019). 'A Wind of Change: Detecting and
Evaluating Lexical Semantic Change across Times and Domains.'
\url{https://arxiv.org/abs/1906.02979v1}.
Shoemark et a. (2019). 'Room to Glo: A Systematic Comparison
of Semantic Change Detection Approaches with Word Embeddings.'
\emph{Proceedings of the 2019 Conference on Empirical Methods in
Natural Language Processing}. 66-76. \doi{10.18653/v1/D19-1007}
Borg and Groenen. (1997). \emph{Modern Multidimensional Scaling}.
New York: Springer. 340-342
}
