# text2map 0.4.0

## Additions
- added `weight_ppmi()` for Positive Pointwise Mutual Information weighting of term co-occurrence matrices; sparse-aware implementation operating on `dgCMatrix` slots directly, with `smooth` parameter for count smoothing before marginal estimation
- added `print.CMDist()`/`plot.CMDist()` S3 methods (small-multiples plot sorted by score, with sensitivity-interval error bars when present)

## Changes
- `CMDist()` now returns a data frame with class `c("CMDist", "data.frame")` instead of plain `"data.frame"`, so that `print.CMDist()`/`plot.CMDist()` dispatch on its output. This is backward compatible — the result is still a real data frame, and all normal data frame operations (`[`, `cbind()`, dplyr pipelines, etc.) work unchanged — but code that checks `identical(class(x), "data.frame")` rather than `inherits(x, "data.frame")` will now see the extra class
- `ClusterR` (used only by `get_regions()`) and `RSpectra` (used only by `doc_centrality(method = "eigen")`) moved from Imports to Suggests with `requireNamespace()` checks, since neither is needed for the package's default/core code paths; `text2vec` and `igraph` remain hard Imports, since they're required by `CMDist()`'s and `CoCA()`'s default behavior respectively — an earlier attempt to make these Suggests too broke under R-hub's `nosuggests` check (missing example/test skip guards) and has been reverted
- added `data-raw/` to `.gitignore`
- added `*.swp`/`*.swo`/`*.swn` to `.gitignore` (replacing an overly narrow `.Rprofile.swp`-only entry) and removed a stale, tracked vim swap file
- added CI jobs for `spelling::spell_check_package()` and a coverage threshold (80%; current coverage ~92%) so the coverage job actually gates instead of only reporting a badge
- added a `lint` CI job with a deliberately curated linter set (`.lintr`: `object_usage_linter`, `seq_linter`, `vector_logic_linter`, and a few others) rather than lintr's full default set, which flags ~2,000 mostly stylistic issues (indentation, line length) on this codebase; added `Language: en-US` and `spelling`/`lintr` to `Suggests`
- simplified a handful of real lint findings: `if (type == "compound" | type == "centroid")` → `||` in `test_anchors.relco()` (safer for scalar conditions), `seq_len(length(x))` → `seq_along(x)`, `any(is.na(x))` → `anyNA(x)`, and two redundant `== TRUE`/`!= TRUE` comparisons on logical vectors — no behavior change
- batched `.prep_cmd_INPUT()`'s vocab-growing step into a single `cbind()` instead of one per missing concept word
- warnings across the package now use `cli::cli_warn()`, matching existing `cli::cli_abort()`/`cli::cli_inform()` usage
- all `stop()` calls across the package now use `cli::cli_abort()` for consistent, better-formatted error messages (error text is unchanged where tests depend on specific wording)
- `doc_centrality()` and `dtm_stats()` now warn on degenerate input (documents with no ties; empty DTMs) instead of silently returning `NaN`
- consolidated the four `.terms_stop_*` internal helpers (`dtm_stopper()`'s term/document frequency and proportion rules) into one shared `.terms_stop_filter()`, removing ~100 lines of duplicated Inf-replacement/rounding/threshold logic; no behavior change
- `dtm_builder()`'s friendly error messages (empty last row; missing doc ID column) are now raised as a single error with the original `Matrix`/`dplyr` error preserved as the cause, instead of a separate `message()` immediately followed by the raw error
- `CoCA()` now warns (previously just informed) when it overrides an explicit `filter_sig = TRUE` because too few directions were supplied
- added `RhpcBLASctl` as an Import and pinned BLAS/OpenMP to a single thread around every `text2vec::RWMD` call (`CMDist()`, `doc_similarity(method = "wmd")`); `text2vec`'s compiled Word Mover's Distance code can deadlock under a multi-threaded BLAS (e.g. OpenBLAS) due to nested OpenMP thread-pool contention, and this can't be worked around with an environment variable set at runtime since the thread pool is already initialized by the time any R code runs

## Corrections
- fixed empty `dtm_stats` test by adding missing assertions
- fixed `dfm` cbind warning: early-return now only for pure `dgCMatrix`, not subclasses like `dfm`
- suppress plot windows in tests with `pdf(NULL)`
- fixed RNG state leaking into the caller's session in `rancor_builder()`/`rancors_builder()` when `seed = NULL`
- fixed `.Random.seed` not found errors in a fresh R session in `perm_tester()`, `test_anchors()` (relco method), and `plot.CoCA()`
- fixed `doc_centrality(method = "span")` incorrectly requiring igraph
- fixed `perm_tester()` crashing with `object 'x' not found` when the model has exactly 2 coefficients (missing `drop = FALSE` in column subset)
- fixed `find_transformation(method = "align")` crashing with `non-conformable arguments` when `wv`/`ref` have different row counts (now validates upfront)
- fixed `dtm_builder()` producing a DTM with the wrong number of rows when the last document had zero tokens (`Matrix::sparseMatrix()` inferred nrow from `max(i)` instead of actual document count; now passes explicit dims)
- registered `relco` print methods (`tbl_sum.relco`, `tbl_format_footer.relco`) as S3 methods; previously silently unused
- fixed `plot.CoCA()` crashing with an unhelpful error on invalid/out-of-range `module`
- fixed `dtm_builder(vocab =, chunk =)` creating a spurious extra chunk row at exact chunk-size boundaries, and silently dropping (instead of bucketing) out-of-vocabulary tokens
- fixed `.dtm_error_handler()` no longer recognizing current `Matrix` package error wording
- fixed wrong argument name in `dtm_stopper(stop_termprop =)`'s error message
- fixed `CMDist()` crashing on single-document DTMs
- fixed `get_direction(method = "PCA")` giving an unrelated error when `n_dirs` exceeds available anchor pairs
- fixed `get_direction()`'s `\examples` calling `method = "PCA"` with only one anchor pair, which failed `R CMD check` after the two-pair minimum above was added
- added `.ontomics` (a local tool directory, not part of the package) to `.Rbuildignore`; was triggering an R CMD check NOTE
- fixed `find_transformation(method = "retrofit")` giving an opaque error when `wv`/`ref` share no vocabulary
- fixed `perm_tester()` giving an opaque error for an invalid `statistic` argument
- fixed `get_centroid()`/`get_direction()` silently corrupting results when `missing = "remove"` dropped an out-of-vocabulary term: an in-place `anchors[] <-` assignment recycled the shortened result back to the original length instead of shrinking, duplicating an earlier term in `get_centroid()` (double-weighting it) and crashing or corrupting `get_direction()` when the row count changed unevenly
- fixed a typo in `find_transformation()`'s documentation ("simplying" → "simply"; "et a." → "et al.")
- fixed a flaky parallel-CMDist test that only failed under core-limited check environments (e.g. `_R_CHECK_LIMIT_CORES_`, as `R CMD check` sets): the test requested more threads than its reduction logic could ever let survive the final row-count check
- fixed `CoCA()` silently computing nonsense (`NaNs produced`) instead of disabling `filter_sig` when given exactly 2 directions: the guard compared `ncol()` of the CMD score data frame (which includes a `doc_id` column) directly against 3, off by one relative to the actual direction count
- fixed `CoCA()` crashing with an opaque `dim(X) must have a positive length` error when given exactly 1 direction; it now errors upfront with a clear message, since document correlations are undefined with fewer than 2 directions
- fixed `CMDist(cw =, missing = "remove")` crashing with an opaque `invalid character indexing` error when a multi-word `cw` phrase (e.g. `"critical thinking"`) had only some of its words missing from the embeddings: `.check_term_in_embeddings()` compared whole phrases against the list of missing individual words, so a partially-missing phrase was never actually dropped from `cw` despite the removal message saying otherwise; it now drops the whole phrase if any of its words is missing
- fixed `CMDist(cv = <0-row matrix>)` crashing with an opaque `'names' must be a character vector` error; it now errors upfront with a clear message
- fixed `weight_ppmi(smooth > 0)` triggering a `Matrix` package deprecation warning (`as(<dgeMatrix>, "dgCMatrix")` is deprecated); now coerces via `"CsparseMatrix"`, matching the pattern used elsewhere in the package
- fixed `CMDist(parallel = TRUE, sens_interval = TRUE)` crashing with `task 1 failed - "invalid first argument"` whenever `threads` made a worker's chunk exactly 1 document: a missing `drop = FALSE` collapsed that 1-row chunk to a bare vector before resampling
- fixed `get_centroids(groups =)` erroring with an opaque `missing value where TRUE/FALSE needed` whenever `groups` contained `NA`; rows with a missing group label are now excluded from every centroid, with a message, matching how empty factor levels are already handled
- fixed `find_projection()`/`find_rejection()` silently returning a matrix of `NaN` for an all-zero `vec`; both now error with a clear message
- fixed `test_anchors(method = "pairdir")` crashing with an opaque `attempt to set 'rownames' on an object with no dimensions` error with exactly 1 anchor pair; it now errors upfront with a clear message, since pairwise cosine similarity is undefined with fewer than 2 pairs
- fixed `find_transformation(method = "retrofit")` crashing with an opaque `inherits(wv, "matrix") ... is not TRUE` error when the shared vocabulary between `wv` and `ref` was smaller than `wv`'s embedding dimension; it now errors upfront with a clear message, since retrofitting needs at least as many shared terms as dimensions to recover
- fixed `get_direction(method = "PCA")` silently returning a meaningless unit vector with only 1 anchor pair (a single centered row degenerates in `prcomp()`); it now errors upfront with a clear message, since PCA needs at least 2 anchor pairs
- fixed `get_regions(k_regions > nrow(wv))` crashing with an opaque `invalid 'data'` error, several steps removed from the real cause (`ClusterR::KMeans_arma` doesn't raise an R condition on this input — it prints a message and returns a character string in place of a matrix); it now errors upfront with a clear message
- `dtm_stopper(stop_termrank =)` now warns when the threshold is at or above the DTM's term count, since this silently removed the entire vocabulary with no indication
- `dtm_builder()` now errors with a clear message when `text` or `doc_id` contains `NA` (previously the `NA` silently became a real column/row name), and when `vocab` is empty or contains duplicate terms (previously produced literal duplicate DTM columns, with later duplicates silently all-zero, or a silent 0-column DTM)
- `doc_centrality(two_mode = FALSE)` now errors with a clear message on a non-square input instead of silently computing meaningless row sums across mismatched columns (only affected `method = "degree"`; `"between"`/`"eigen"`/`"span"` already failed loudly via their underlying dependency)

# text2map 0.3.0

# text2map 0.2.3

## Additions
- adding `relco` method to `test_anchors`

## Corrections
- fix error in calculating sig cutoff values for CoCA (Thanks to Bianca Kang!)

# text2map 0.2.0

- fix testing Suggests for CRAN
- minor change to output labeling for `test_anchor` 
- updated documentation for `test_anchor`
- added vignette for `test_anchor`


# text2map 0.1.9.9000

- minor change to output labeling for `test_anchor` 
- updated documentation for `test_anchor`
- added vignette for `test_anchor`

# text2map 0.1.9

Added `test_anchor` 
Added more unit tests
Fixed a bug in `doc_centrality` using the `centroid` method


# text2map 0.1.8

Fixes for changes to the Matrix package
Updating documentation and added examples

# text2map 0.1.6.9003

Fix encoding issue for non-ASCII characters to work with `fastmatch`
Add functionality 
- `perm_tester` for Monte Carlo Permutation Tests for Model P-Values
- `rancor_builder` creates random corpus based on provided term probabilities 
- `rancors_builder` creates multiple random corpora

Include additional tests, updated documentation and vignettes

# text2map 0.1.6.9001

Working on an encoding error in `fastmatch` which shows inconsistent behavior with non-ASCII characters. This dev version provides a temporary fix.

# text2map 0.1.6
## Improvements

- Add functionality
  - `doc_centrality` calculates four graph-based centrality metrics using DTMs
  - `doc_similarity` calculates four document similarity measures using DTMs


# text2map 0.1.5
## Improvements

- Replaced dependency
  - using ClusterR for `get_regions`, instead of mlpack
  - Uses the Armadillo library k-means algorithm only (no longer provides an option)
- Added functionality:
  - `seq_builder` creates a token-integer sequence representation
- Added Shakespeare metadata for examples
- Import Matrix package methods

# text2map 0.1.4

## Improvements

* Added functionality
    - `dtm_builder` includes an option to return a dense base R matrix
    - `dtm_stopper` includes an option to remove based on a terms rank (e.g., top 10), stopping based on count and proportion are now two separate options

# text2map 0.1.3

## Improvements

* Add functions:
    - `find_transformation()` to norm, center, and align matrices
    - `find_projection()` finds the projection matrix onto a vector
    - `find_rejection()` finds the rejection matrix away from a vector
    - `dtm_melter()` quickly turns a DTM into a triplet dataframe (doc_id, term, count)
* Fixed `get_centroid()` naming (limits to single word for names)

# text2map 0.1.1

## Improvements

* Added functionality to `dtm_stopper()` to stop words by document or term frequencies
    * Nomenclature was changed, `stop_freq` was changed to `stop_termfreq`
* Added functionality to `dtm_resampler()` to resample proportion and fixed N lengths
* Added and clarified documentation
* Added a `NEWS.md` file to track changes to the package.
