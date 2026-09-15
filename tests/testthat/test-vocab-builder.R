test_that("vocab_builder returns character vector of unique terms", {
    out <- vocab_builder(my_corpus, clean_text)
    expect_type(out, "character")
    expect_true(length(out) > 0)
    expect_true(!anyDuplicated(out))
})

test_that("vocab_builder preserves first-appearance order", {
    out <- vocab_builder(my_corpus, clean_text)
    expect_true(out[1] == "i")
    expect_true("hear" %in% out)
})

test_that("vocab_builder works with single row data frame", {
    single_df <- data.frame(text = "hello world hello", id = "a")
    out <- vocab_builder(single_df, text)
    expect_identical(out, c("hello", "world"))
})

test_that("vocab_builder with jfk corpus matches dtm columns", {
    out <- vocab_builder(jfk_corpus, clean_text)
    expect_true(length(out) > 0)
    expect_type(out, "character")
    dtm <- dtm_builder(jfk_corpus, clean_text, doc_id)
    expect_true(all(colnames(dtm) %in% out))
})

test_that("vocab_builder handles empty strings", {
    empty_df <- data.frame(text = c("", "hello world", ""), id = c("a", "b", "c"))
    out <- vocab_builder(empty_df, text)
    expect_type(out, "character")
    expect_true("hello" %in% out)
    expect_true("world" %in% out)
})

test_that("vocab_builder with whitespace-only strings produces no extra terms", {
    ws_corpus <- data.frame(text = c("hello world", "   "), id = c("a", "b"))
    out <- vocab_builder(ws_corpus, text)
    expect_true("hello" %in% out)
    expect_true("world" %in% out)
    expect_false(" " %in% out)
})

test_that("vocab_builder removes duplicates preserving first-appearance order", {
    df <- data.frame(text = c("banana apple cherry", "apple banana"), id = c("a", "b"))
    out <- vocab_builder(df, text)
    expect_identical(out, c("banana", "apple", "cherry"))
})