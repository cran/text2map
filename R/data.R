#' A dataset of stoplists
#'
#' A dataset containing eight English stoplists. Is used
#' with the [get_stoplist()] function.
#'
#' @details
#'
#' The stoplists include:
#'
#' - "tiny2020": Tiny (2020) list of 33 words (Default)
#' - "snowball2001": Snowball (2001) list of 127 words
#' - "snowball2014": Updated Snowball (2014) list of 175 words
#' - "van1979": van Rijsbergen's (1979) list of 250 words
#' - "fox1990": Christopher Fox's (1990) list of 421 words
#' - "smart1993": Original SMART (1993) list of 570 words
#' - "onix2000": ONIX (2000) list of 196 words
#' - "nltk2009": Python's NLTK (2009) list of 179 words
#'
#' Tiny 2020, is a very small stop list of the most frequent
#' English conjunctions, articles, prepositions, and
#' demonstratives (N=17). Also includes the 8 forms of the
#' copular verb "to be" and the 8 most frequent personal
#' (singular and plural) pronouns (minus gendered
#' and possessive pronouns).
#'
#' No contractions are included.
#'
#' @section Variables:
#' Variables:
#' \itemize{
#' \item words. words to be stopped
#' \item source. source of the list
#'
#' }
#'
#' @docType data
#' @name stoplists
#' @keywords datasets
#' @format A data frame with 1775 rows and 2 variables.
#' @source
#' - tiny2020: Stoltz and Taylor (2020)
#' - snowball2001: Porter (2001) Snowball stemming algorithm
#' - snowball2014: Porter (2014) updated Snowball stoplist
#' - van1979: van Rijsbergen (1979) "Information Retrieval"
#' - fox1990: Fox (1990) "A Stop List for General Text"
#' - smart1993: Salton and Buckley (1993) SMART retrieval system
#' - onix2000: ONIX (2000) "Oxford English Dictionary" stoplist
#' - nltk2009: Bird, Loper and Klein (2009) NLTK
NULL


#' A dataset of anchor lists
#'
#' A dataset containing juxtaposing pairs of English words
#' for 30 semantic relations. These anchors are used with
#' the [get_anchors()] function, which can then be used with
#' the [get_direction()] function. These have been collected
#' from previously published articles and should be used
#' as a starting point for defining a given relation in
#' a word embedding model.
#'
#' @section Variables:
#' Variables:
#' \itemize{
#' \item pole1. words to be added (or the positive direction)
#' \item pole2. words to be subtracted (or the negative direction)
#' \item relation. the relation to be extracted, 30 relations available
#' \item domain. 6 broader categories within which each relation falls
#'
#' }
#' @seealso [CoCA], [get_direction], [get_centroid], [get_anchors]
#'
#' @docType data
#' @name anchor_lists
#' @keywords datasets
#' @format A data frame with 303 rows and 4 variables.
#' @source
#' Curated from previously published semantic direction anchor sets.
#' See: Kozlowski et al. (2019) "The Geometry of Culture";
#' Garg et al. (2018) "Word Embeddings Quantify 100 Years of Gender and Ethnic Stereotypes";
#' Bolukbasi et al. (2016) "Man is to Computer Programmer as Woman is to Homemaker".
NULL

#' Full Text of JFK's Rice Speech
#'
#' This is a data frame for the text of JFK's Rice Speech "We choose to go to
#' the moon." Each row is a 10 word string of the speech -- roughly a sentence.
#' This is intended to be used for example code.
#'
#' @section Variables:
#' Variables:
#' \itemize{
#' \item sentence_id. Order and unique ID for the sentence
#' \item sentence. The text of a sentence
#' }
#'
#' @docType data
#' @name jfk_speech
#' @keywords datasets
#' @format A data frame with 84 rows and 2 columns
#' @source
#' John F. Kennedy, "We choose to go to the Moon" speech,
#' Rice University, September 12, 1962. Public domain.
NULL

#' Sample of fastText embeddings
#'
#' These are a sample of the English fastText embeddings
#' including 770 words matching those used in the `jfk_speech`.
#' These are intended to be used for example code.
#'
#' @docType data
#' @name ft_wv_sample
#' @keywords datasets
#' @format A matrix of 770 rows and 300 columns
#' @source
#' Subsample of English fastText word embeddings.
#' Bojanowski et al. (2017) "Enriching Word Vectors with Subword Information."
#' \doi{10.1162/tacl_a_00051}. Available at \url{https://fasttext.cc}.
NULL


#' Metadata for Shakespeare's First Folio
#'
#' Metadata related to Shakespeare's First Folio
#' including the IDs to download the plays from
#' Project Gutenberg, and a count of the number of
#' deaths in each play (body count).
#' 
#' @section Variables:
#' Variables:
#' \itemize{
#' \item short_title.
#' \item gutenberg_title.
#' \item gutenberg_id.
#' \item genre.
#' \item year.
#' \item body_count.
#' \item boas_problem_plays.
#' \item death.
#' }
#'
#' @docType data
#' @name meta_shakespeare
#' @keywords datasets
#' @format A data frame with 37 rows and 8 columns
#' @source
#' Shakespeare's First Folio plays, obtained via Project Gutenberg
#' (\url{https://www.gutenberg.org}). Body counts compiled by
#' Stoltz and Taylor (2021) "Integrating Semantic Contrast with
#' Concept Mover's Distance" \doi{10.1016/j.poetic.2021.101567}.
NULL
