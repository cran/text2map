## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=TRUE, message=FALSE, out.width='85%', echo=FALSE-------------------
knitr::include_graphics("figures/02_fig1_cmd.png")

## ---- eval = FALSE, message = FALSE-------------------------------------------
#  
#  library(text2map)
#  
#  library(text2vec)
#  library(gutenbergr)
#  library(tidyverse)
#  library(textclean)
#  library(stringi)
#  

## ---- eval = FALSE, message = FALSE-------------------------------------------
#  
#  data("meta_shakespeare")
#  # Grab the text from Project GUTENBERG
#  df_plays <- meta_shakespeare |>
#    dplyr::select(gutenberg_id) |>
#    gutenberg_download() |>
#    group_by(gutenberg_id) |>
#    summarize(text = paste(text, collapse = ", "))
#  

## ---- eval = FALSE------------------------------------------------------------
#  
#  df_plays <- df_plays |> mutate(
#    ## transliterate and lowercase
#    text = replace_curly_quote(text),
#    text = stri_trans_general(text, id = "Any-Latin; Latin-ASCII"),
#    text = tolower(text),
#    ## punctuation
#    text = gsub("(\\w+[_'-]+\\w+)|[[:punct:]]+", "\\1", text),
#    text = replace_contraction(text),
#    ## numbers and spaces
#    text = gsub("[[:digit:]]+", " ", text),
#    text = gsub("[[:space:]]+", " ", text),
#    text = trimws(text)
#  )
#  

## ---- eval = FALSE------------------------------------------------------------
#  
#  dtm_plays <- df_plays |>
#    dtm_builder(text, gutenberg_id)

## ----eval = FALSE-------------------------------------------------------------
#  
#  # install if necessary
#  remotes::install_gitlab("culturalcartography/text2map.pretrained")
#  
#  library(text2map.pretrained)
#  
#  # download once per machine
#  download_pretrained("vecs_fasttext300_wiki_news")
#  
#  # load with data() once per session
#  data("vecs_fasttext300_wiki_news")
#  
#  # it is a wordy, descriptive name, so you can rename it
#  my_wv <- vecs_fasttext300_wiki_news
#  rm(vecs_fasttext300_wiki_news)
#  
#  

## ---- eval = FALSE------------------------------------------------------------
#  
#  cos_sim <- sim2(
#    x = my_wv,
#    y = my_wv["thinking", , drop = FALSE],
#    method = "cosine"
#  )
#  
#  head(sort(cos_sim[, 1], decreasing = TRUE), 10)

## ---- eval = FALSE------------------------------------------------------------
#  
#  doc_closeness <- CMDist(dtm = dtm_plays, cw = "thinking", wv = my_wv)

## ---- eval = FALSE------------------------------------------------------------
#  
#  doc_closeness <- CMDist(dtm = dtm_plays, cw = "critical thinking", wv = my_wv)

## ---- eval = FALSE------------------------------------------------------------
#  
#  doc_closeness <- CMDist(dtm = dtm_plays, cw = "critical-thinking", wv = my_wv)

## ---- eval = FALSE------------------------------------------------------------
#  # first build the semantic direction:
#  additions  <- c("death", "casualty", "demise", "dying", "fatality")
#  substracts <- c("life", "survivor", "birth", "living", "endure")
#  
#  pairs <- cbind(additions, substracts)
#  sd_death <- get_direction(pairs, my_wv)
#  
#  # input it into the function just like a concept word:
#  doc_closeness <- CMDist(dtm = dtm_plays, cv = sd_death, wv = my_wv)
#  

## ---- eval = FALSE------------------------------------------------------------
#  # first build the semantic centroid:
#  terms  <- c("death", "casualty", "demise", "dying", "fatality")
#  
#  sc_death <- get_centroid(terms, my_wv)
#  
#  # input it into the function just like a concept word:
#  doc_closeness <- CMDist(dtm = dtm_plays, cv = sc_death, wv = my_wv)
#  

## ---- eval = FALSE------------------------------------------------------------
#  
#  # example 1
#  concept_words <- c("thinking", "critical", "thought")
#  doc_closeness <- CMDist(dtm = dtm_plays, cw = concept_words, wv = my_wv)
#  
#  # example 2
#  concept_words <- c("critical thought", "critical-thinking")
#  doc_closeness <- CMDist(dtm = dtm_plays, cw = concept_words, wv = my_wv)
#  
#  # example 3
#  concept_words <- c("critical thought", "critical-thinking")
#  terms  <- c("death", "casualty", "demise", "dying", "fatality")
#  concept_vectors <- get_centroid(terms, my_wv)
#  doc_closeness <- CMDist(dtm = dtm_plays, cw = concept_words, cv = concept_vectors, wv = my_wv)
#  

## ---- eval = FALSE------------------------------------------------------------
#  doc_closeness <- CMDist(dtm = dtm_plays,
#                        cw = "critical-thinking",
#                        wv = my_wv,
#                        parallel = TRUE,
#                        threads = 2)
#  

## ---- eval = FALSE------------------------------------------------------------
#  
#  doc_closeness <- CMDist(dtm = dtm_plays, cw = "thinking", wv = my_wv, scale = FALSE)
#  

