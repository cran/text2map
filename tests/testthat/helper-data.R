
# Helper functions for testhat
#' @importFrom tidytext unnest_tokens cast_dtm cast_dfm
#' @importFrom dplyr count pipe
#' @importFrom stringi stri_trim_both
#' @importFrom testthat
#' 

require(tm)
require(quanteda)

## -----------------------------------------------------------------------------
# create some text
jfk_text <- c("We choose to go to the moon. We choose to go to the moon in this
           decade and do the other things, not because they are easy, but
           because they are hard, because that goal will serve to organize
           and measure the best of our energies and skills, because that
           challenge is one that we are willing to accept, one we are unwilling
           to postpone, and one which we intend to win, and the others, too.")
cleaned <- gsub("[,.]+", "", tolower(jfk_text) )
cleaned <- stringi::stri_trim_both(gsub("[\n]", "", cleaned))
words <- unique(unlist(strsplit(cleaned, " ")))

# Create minicorpus
sentences <- strsplit(jfk_text, "[,.]")[[1]]
sentences <- stringi::stri_trim_both(gsub("[\n]", "", sentences))
jfk_corpus <- data.frame(
    text = sentences,
    clean_text = tolower(gsub("'", "", sentences)),
    doc_id = LETTERS[seq_len(length(sentences))]
)


# create another mini corpus
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
# some text preprocessing
my_corpus$clean_text <- tolower(gsub("'", "", my_corpus$text))

# -----------------------------------------------------------------------------
### Create fake word vectors ##
# -----------------------------------------------------------------------------
# 10 Dimensions and 30 Words
# set.seed(59801);rnorm(300) |> round(.,6) -> rnums
# fake_word_vectors <-matrix(rnums, nrow=30)
# rownames(fake_word_vectors) <- words[1:30]
# saveRDS(fake_word_vectors, "tests/testthat/fake_word_vectors.Rds")
#
# # Create fake word vectors with out of vocab words
# set.seed(59801);rnorm(350) |> round(.,6) -> rnums
# fake_word_vectors_oov <-matrix(rnums, nrow=35)
# rownames(fake_word_vectors_oov) <- c(words[1:30],
#                                      "victory", "back",
#                                      "retirement", "item",
#                                      "moving")
# saveRDS(fake_word_vectors_oov, "tests/testthat/fake_word_vectors_oov.Rds")
# fake_word_vectors <- readRDS("tests/testthat/fake_word_vectors.Rds")
# fake_word_vectors_oov <- readRDS("tests/testthat/fake_word_vectors_oov.Rds")

fake_word_vectors <- readRDS("fake_word_vectors.Rds")
fake_word_vectors_oov <- readRDS("fake_word_vectors_oov.Rds")

# Create fake word vectors with out of vocab words
# set.seed(46556);rnorm(400) |> round(6) -> rnums
# fake_word_vectors_coca <- matrix(rnums, nrow=40)
# rownames(fake_word_vectors_coca) <- c(words[1:30], new.words)
# saveRDS(fake_word_vectors_coca, "tests/testthat/fake_word_vectors_coca.Rds")
# fake_word_vectors_coca <- readRDS("tests/testthat/fake_word_vectors_coca.Rds")
fake_word_vectors_coca <- readRDS("fake_word_vectors_coca.Rds")


# create concept word vector
cw <- c("choose")
cw.2 <- c("choose", "decade")
cw.3 <- c("choose", "decade", "the moon")
cw.4 <- c("choose", "decade", "decade moon")
# create concept vector not in DTM
cw.oov <- c("victory")
#
wv.dims <- c(as.integer(nrow(fake_word_vectors)),
             as.integer(ncol(fake_word_vectors)))

wv.oov.dims <- c(as.integer(nrow(fake_word_vectors_oov) - 4),
                 as.integer(ncol(fake_word_vectors_oov)))

# -----------------------------------------------------------------------------
## create fake anchors
# -----------------------------------------------------------------------------

## Single
# character list
anchor.solo.c <- c("choose", "moon")
anchor.solo.d <- c("moon", "choose")

# actual list list
anchor.solo.list <- list(c("choose", "moon"))
# data.frame
anchor.solo.df <- data.frame(add = c("choose", "moon"))
# tibble
anchor.solo.tbl <- tibble::tibble(add = c("choose", "moon"))

## Paired ## paired list of terms ##
# actual list list
# anchors <- list(add=c("choose", "moon"),
#                 sub=c("decade", "this"))

anchor.pair.list <- list(add=c("choose", "moon"),
                         sub=c("decade", "this"))
# data.frame
anchor.pair.df <- data.frame(add=c("choose", "moon"),
                             sub=c("decade", "this"))
# tibble
anchor.pair.tbl <- tibble::tibble(add=c("choose", "moon"),
                                  sub=c("decade", "this"))


# expected dimensions
ce.dims <- c(1L, as.integer(ncol(fake_word_vectors)))
di.dims <- c(1L, as.integer(ncol(fake_word_vectors)))

ce.name <- "choose_centroid"
di.name <- "choose_pole"

wv.cv.dims <- c(as.integer(nrow(fake_word_vectors)+1),
                as.integer(ncol(fake_word_vectors)))

wv.cv.cw.dims <- c(as.integer(nrow(fake_word_vectors)+2),
                as.integer(ncol(fake_word_vectors)))

new.words <- c("rich", "richer", "affluence",
                "poor", "poorer", "poverty",
                "skilled", "competent",
                "unskilled", "incompetent") #14



# build juxtaposed pairs for each semantic directions
pairs.01 <- data.frame(additions  = c("rich", "richer", "affluence"),
                        substracts = c("poor", "poorer", "poverty") )

pairs.02 <- data.frame(additions  = c("skilled", "competent"),
                        substracts = c("unskilled", "incompetent") )


# get the vectors for each direction
sd.01 <- get_direction(pairs.01, fake_word_vectors_coca )
sd.02 <- get_direction(pairs.02, fake_word_vectors_coca )

# row bind each direction
sem.dirs <- rbind(sd.01, sd.02)



# -----------------------------------------------------------------------------
## create fake dtm
# -----------------------------------------------------------------------------

# dtm_base_loop <- function(text, doc_id){
#     tokns <- strsplit(tolower(text), " ", fixed=TRUE)
#     vocab <- sort(unique(unlist(tokns)))
#     dtm <- matrix(data = 0L,
#                   ncol = length(vocab), nrow = length(tokns),
#                   dimnames = list(doc_id, vocab) )
#     freqs <- lapply(tokns, table)
#     for (i in seq_len(length(freqs)) ){
#                 doc <- freqs[[i]]
#                 dtm[i, names(doc)] <- as.integer(doc)
#     }
#     return(dtm)
# }


# dtm_simple_sparse  <- function(text, doc_id){
#         tokns <- strsplit(tolower(text), " ")
#         vects <- unlist(tokns)
#         vocab <- sort(unique(vects))
#         lens <- vapply(tokns, length, numeric(1L))
#         dtm <- Matrix::sparseMatrix(i=rep(seq_along(lens), lens),
#                                     j=match(vects, vocab), x=1L,
#                                     dimnames = list(doc_id, vocab))
#   }

# tidytext_dtm <- function(df_text) {
#             dtm <- df_text |> tidytext::unnest_tokens(word, text) |>
#                                 dplyr::count(doc_id, word, sort = TRUE) |>
#                                 tidytext::cast_dtm(doc_id, word, n)
# }

# tidytext_tdm <- function(df_text) {
#   dtm <- df_text |> tidytext::unnest_tokens(word, text) |>
#     dplyr::count(doc_id, word, sort = TRUE) |>
#     tidytext::cast_tdm(doc_id, word, n)
# }

# tidytext_dfm <- function(df_text) {
#             dfm <- df_text |> tidytext::unnest_tokens(word, text) |>
#                 dplyr::count(doc_id, word, sort = TRUE) |>
#                 tidytext::cast_dfm(doc_id, word, n)
# }

# # base matrix
# dtm.bse <- dtm_builder(jfk_corpus, text, doc_id, dense = TRUE)
# saveRDS(dtm.bse, "tests/testthat/fake_dtm_bse.Rds")

# # sparse __dgCMatrix__ matrix
# # corpustools::get_dtm()
# # text2vec::create_dtm()
# # textmineR::CreateDtm()
# # textTinyR::sparse_term_matrix
# # wactor::dtm()
# # udpipe::document_term_matrix()
# #dtm.dgc <- dtm_simple_sparse(jfk_corpus$text, jfk_corpus$doc_id)
# dtm.dgc <- dtm_builder(jfk_corpus, text, doc_id)
# saveRDS(dtm.dgc, "tests/testthat/fake_dtm_dgc.Rds")

# # class __dfm__ matrix
# # tidytext::cast_dfm()
# # quanteda::dfm()
# # corpustools::get_dfm()
# dtm.dfm <- tidytext_dfm(jfk_corpus)
# saveRDS(dtm.dfm, "tests/testthat/fake_dtm_dfm.Rds")

# # sparse __simple_triplet_matrix__ matrix
# # gofastr::q_dtm() # also class __DocumentTermMatrix__
# # tidytext::cast_dtm() # also class __DocumentTermMatrix__
# # tm::DocumentTermMatrix() # also class __DocumentTermMatrix__
# dtm.tm <- tidytext_dtm(jfk_corpus)
# saveRDS(dtm.tm, "tests/testthat/fake_dtm_tm.Rds")

# # sparse __simple_triplet_matrix__ matrix
# # tm::TermDocumentMatrix() # TermDocumentMatrx
# dtm.tdm <- tidytext_tdm(jfk_corpus)
# saveRDS(dtm.tdm, "tests/testthat/fake_dtm_tdm.Rds")

dtm.bse <- readRDS("fake_dtm_bse.Rds")
dtm.dgc <- readRDS("fake_dtm_dgc.Rds")
dtm.dfm <- readRDS("fake_dtm_dfm.Rds")
dtm.tm  <- readRDS("fake_dtm_tm.Rds")
dtm.tdm <- readRDS("fake_dtm_tdm.Rds")

# create JFK dtm
# # base matrix
# dtm_bse_jfk <- dtm_builder(jfk_corpus, text, doc_id, dense = TRUE)
# saveRDS(dtm_bse_jfk, "tests/testthat/fake_dtm_bse_jfk.Rds")
# dtm_bse_jfk <- readRDS("fake_dtm_bse.Rds")


# -----------------------------------------------------------------------------
# Create TCM
# -----------------------------------------------------------------------------

# tkns <- quanteda::tokens(jfk_corpus$text)
# tcm <- quanteda::fcm(tkns,
#     context = "window",
#     window = 5L,
#     tri = FALSE
# )

# # tcm.dgc <- as(tcm, "dgCMatrix")
# tcm.dgc <- methods::as(
#       methods::as(
#           methods::as(tcm, "dMatrix"),
#           "generalMatrix"
#       ), "CsparseMatrix"
#   )


# saveRDS(tcm, "tests/testthat/fake_tcm_quanteda.Rds")
# saveRDS(tcm.dgc, "tests/testthat/fake_tcm_quanteda.Rds")

tcm <- readRDS("fake_tcm_quanteda.Rds")
tcm.dgc <- readRDS("fake_tcm_quanteda.Rds")


# -----------------------------------------------------------------------------
## FOR CoCA
# -----------------------------------------------------------------------------

# Get CoCA print message
cor.dims <- c(as.integer(nrow(dtm.bse)),
              as.integer(nrow(dtm.bse)))

