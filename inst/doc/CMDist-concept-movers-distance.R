## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=TRUE, message=FALSE, out.width='85%', echo=FALSE-------------------
knitr::include_graphics("figures/02_fig1_cmd.png")

## ----eval = FALSE-------------------------------------------------------------
#  library(googledrive) # (see https://googledrive.tidyverse.org/)
#  temp <- tempfile()
#  drive_download(as_id("17H4GOGedeGo0urQdDC-4e5qWQMeWLpGG"), path = temp, overwrite = TRUE)
#  my.wv <- readRDS(temp)
#  # save them to your project file so you don't have to re-download
#  saveRDS(my.wv, "data/ft.cc.en.300D.2M.Rds")

## ---- eval = FALSE------------------------------------------------------------
#  library(text2vec)
#  
#  cos_sim <- sim2(x = my.wv,
#                  y = my.wv["thinking", , drop = FALSE],
#                  method = "cosine")
#  
#  head(sort(cos_sim[,1], decreasing = TRUE), 10)
#  

