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

test_that("tiny_gender_tagger returns a dictionary with categories", {
    text_num <- as.numeric(1.00967)
    expect_identical(.n_decimal_places(text_num), as.integer(5))

    text_num <- as.numeric(1.009671)
    expect_identical(.n_decimal_places(text_num), as.integer(6))

    text_num <- as.numeric(1)
    expect_identical(.n_decimal_places(text_num), as.integer(0))
})
