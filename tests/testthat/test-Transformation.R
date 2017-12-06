context("Transformations")

test_that("removePunctuation works in latin1 locale", {
  if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", "en_US.iso88591")))) {
    id <- c(73L, 108L, 32L, 115L, 39L, 101L, 120L, 112L, 114L, 105L, 109L, 97L,
            105L, 116L,  32L, 101L, 110L, 32L, 117L, 110L, 32L, 108L, 97L, 110L,
            103L,  97L, 103L, 101L,  32L,  99L, 104L, 226L, 116L, 105L, 233L)
    iu <- intToUtf8(id)
    il <- iconv(iu, from = "UTF-8", to = "latin1")
    td <- id[-5L]
    tu <- intToUtf8(td)
    tl <- iconv(tu, from = "UTF-8", to = "latin1")

    expect_equal(removePunctuation(iu), tu)
    expect_equal(removePunctuation(il), tl)
  } else
    skip("latin1 locale not available")
})
