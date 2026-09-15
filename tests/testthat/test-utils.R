test_that("get_stoplist returns stop lists", {
    out <- get_stoplist()
    expect_equal(length(out), 33L)
    expect_type(out, "character")

    out <- get_stoplist(source = "snowball2014")
    expect_equal(length(out), 175L)
    expect_type(out, "character")

    out <- get_stoplist(source = "snowball2001")
    expect_equal(length(out), 127L)
    expect_type(out, "character")

    out <- get_stoplist(source = "fox1990")
    expect_equal(length(out), 421L)
    expect_type(out, "character")

    out <- get_stoplist(source = "nltk2009")
    expect_equal(length(out), 179L)
    expect_type(out, "character")

    out <- get_stoplist(source = "smart1993")
    expect_equal(length(out), 570L)
    expect_type(out, "character")

    out <- get_stoplist(source = "van1979")
    expect_equal(length(out), 250L)
    expect_type(out, "character")

    out <- get_stoplist(source = "onix2000")
    expect_equal(length(out), 196L)
    expect_type(out, "character")

    # as tidy
    out <- get_stoplist(tidy = TRUE)
    expect_equal(dim(out), as.integer(c(33L, 2L)))
    expect_type(out, "list")

    out <- get_stoplist(source = "snowball2014", tidy = TRUE)
    expect_equal(dim(out), as.integer(c(175L, 2L)))
    expect_type(out, "list")

    out <- get_stoplist(source = "fox1990", tidy = TRUE)
    expect_equal(dim(out), as.integer(c(421L, 2L)))
    expect_type(out, "list")

    out <- get_stoplist(source = "snowball2014", tidy = TRUE)
    expect_equal(dim(out), as.integer(c(175L, 2L)))
    expect_type(out, "list")
})

test_that("get_stoplist returns errors", {
    expect_error(
        expect_message(get_stoplist(
            source = "snowball2014",
            language = "fr"
        ))
    )
    expect_error(
        expect_message(get_stoplist(
            source = "picklespit",
            language = "en"
        ))
    )
})

test_that("tiny_gender_tagger returns a dictionary with categories", {
    out <- tiny_gender_tagger()
    expect_equal(dim(out), as.integer(c(40L, 2L)))
    expect_type(out, "list")
})

test_that("tiny_gender_tagger spot checks", {
    out <- tiny_gender_tagger()
    expect_equal(out$gender[out$word == "she"], "feminine")
    expect_equal(out$gender[out$word == "he"], "masculine")
    expect_equal(out$gender[out$word == "woman"], "feminine")
    expect_equal(out$gender[out$word == "man"], "masculine")
    expect_identical(colnames(out), c("word", "gender"))
})

test_that("tiny_gender_tagger returns a dictionary with categories", {
    text_num <- as.numeric(1.00967)
    expect_identical(.n_decimal_places(text_num), as.integer(5))

    text_num <- as.numeric(1.009671)
    expect_identical(.n_decimal_places(text_num), as.integer(6))

    text_num <- as.numeric(1)
    expect_identical(.n_decimal_places(text_num), as.integer(0))
})

test_that("get_stoplist tidy output has word and lexicon columns", {
    out <- get_stoplist(tidy = TRUE)
    expect_s3_class(out, "data.frame")
    expect_identical(colnames(out), c("word", "lexicon"))
    expect_true(nrow(out) > 0)
})

test_that("get_stoplist all sources return character vectors", {
    sources <- c("van1979", "fox1990", "smart1993", "onix2000",
                 "snowball2001", "snowball2014", "tiny2020", "nltk2009")
    for (src in sources) {
        out <- get_stoplist(source = src)
        expect_type(out, "character")
        expect_true(length(out) > 0)
    }
})
