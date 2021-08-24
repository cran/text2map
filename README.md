# [`text2map`](https://culturalcartography.gitlab.io/text2map/): R Tools for Text Matrices <img src="man/figures/logo.png" align="right" height="120" />

This is an R Package (**in development**) with libraries and utility functions for computational text analysis for the social sciences.

The functions are optimized for working with various kinds of **text matrices**. Focusing on the text matrix as the primary object -- which is represented either as a base `R` dense matrix or a `Matrix` package sparse matrix -- allows for a consistent and intuitive interface that stays close to the underlying mathematical foundation of computational text analysis. In particular, the package includes functions for working with word embeddings, text networks, and document-term matrices.

Related text datasets are available on GitLab in text2map.corpora and text dictionaries are available in text2map.dictionaries.

### Installation 

Install the latest development version from GitLab:
``` r
devtools::install_gitlab("culturalcartography/text2map")
```

### Related Packages

There are three related packages in development, and hosted on GitLab: 

- text2map.corpora: collection of text datasets
- text2map.dictionaries: collection of dictionaries:
    - Sensorimotor Norms Dictionary for English, N = 40,000 (Lynott, et al. 2020)
    - Concreteness Dictionary for English, N = 40,000 (Brysbaert et al. 2014)
    - NRC Valence, Arousal, and Dominance Dictionary for English (Mohammad et al. 2018)
- text2map.theme: changes ggplot2 aesthetics and loads viridis color scheme

<!-- badges: start -->

[![pipeline](https://gitlab.com/culturalcartography/text2map/badges/master/pipeline.svg)](https://gitlab.com/culturalcartography/text2map/-/commits/master)

[![coverage](https://gitlab.com/culturalcartography/text2map/badges/master/coverage.svg)](https://gitlab.com/culturalcartography/text2map/-/commits/master)

[![repo status](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

[![license](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT) 

<!-- badges: end -->
