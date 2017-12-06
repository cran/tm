context("Tokenizers")

test_that("scan_tokenizer works with character vectors", {
  tokens <-
      c("a", "character", "vector", "consisting", "of", "multiple", "elements")
  expect_equal(scan_tokenizer(c(paste0(tokens[1:3], collapse = " "),
                                paste0(tokens[4:5], collapse = " "),
                                paste0(tokens[6:7], collapse = " "))), tokens)
})
