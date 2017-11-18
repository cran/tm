context("Sources")

test_that("DataframeSource works", {
  txt <- c("First document.", "Second document.")
  dm1 <- 1:2
  dm2 <- letters[1:2]
  df <- data.frame(doc_id = c("doc_1", "doc_2"), text = txt,
                   dmeta1 = dm1, dmeta2 = dm2, stringsAsFactors = FALSE)
  ds <- DataframeSource(df)
  scorpus <- Corpus(ds)
  vcorpus <- VCorpus(ds)
  expect_equal(as.character(scorpus[[2]]), as.character(vcorpus[[2]]))
  expect_equal(as.character(scorpus[[2]]), txt[2])
  expect_equal(meta(scorpus), meta(vcorpus))
  expect_equal(meta(scorpus),
               data.frame(dmeta1 = dm1, dmeta2 = dm2, stringsAsFactors = FALSE))
})
