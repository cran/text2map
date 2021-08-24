#' A dataset of stoplists
#'
#' A dataset containing eight English stoplists:
#' - "tiny2020": Tiny (2020) list of 33 words (Default)
#' - "snowball2001": Snowball stemming package's (2001) list of 127 words
#' - "snowball2014": Updated Snowball (2014) list of 175 words
#' - "van1979": C. J. van Rijsbergen's (1979) list of 250 words
#' - "fox1990": Christopher Fox’s (1990) list of 421 words
#' - "smart1993": Original SMART (1993) list of 570 words
#' - "onix2000": ONIX (2000) list of 196 words
#' - "nltk2001": Python's NLTK (2009) list of 179 words
#'
#' @details
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
#' @usage stoplists
#' @keywords datasets
#' @format A data frame with 1775 rows and 2 variables.
NULL


#' A dataset of anchor lists
#'
#' A dataset containing juxtaposing pairs of English words
#' for 26 semantic relations. These anchors are used with
#' the `get_anchor()` function, which can then be used with
#' the `get_direction()` function.
#'
#' @section Variables:
#' Variables:
#' \itemize{
#' \item add. words to added (or the positive direction)
#' \item subtract. words to be subtract (or the negative direction)
#' \item relation. the relation to be extracted, 26 relations available
#' \item domain. 6 broader categories within which each relation falls
#'
#' }
#'
#' @docType data
#' @name anchor_lists
#' @usage anchor_lists
#' @keywords datasets
#' @format A data frame with 303 rows and 4 variables.
NULL

#' Full Text of JFK's Rice Speech
#'
#' This is a data frame for the text of JFK's Rice Speech "We choose to go to
#' the moon." Each row is a 10 word string of the speech -- roughly a sentence.
#' It is intended to be used for example code.
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
#' @usage jfk_speech
#' @keywords datasets
#' @format A data frame with 2 columns
NULL

#' Sample of fastText embeddings
#'
#' These are a sample of the English fastText embeddings
#' including 770 words matching those used in the `jfk_speech`.
#' These are intended to be used for example code.
#'
#' @docType data
#' @name ft_wv_sample
#' @usage ft_wv_sample
#' @keywords datasets
#' @format A matrix of 770 rows and 300 columns
NULL