context("Term-document matrices")

test_that("construction works", {
  vs <- VectorSource(c("one two two three three three",
                       "This is a short text with a few words"))
  scorpus <- Corpus(vs)
  vcorpus <- VCorpus(vs)
  ms <- TermDocumentMatrix(scorpus)
  mv <- TermDocumentMatrix(vcorpus)
  terms <- c("few", "one", "short", "text", "this",
             "three", "two", "with", "words")
  docs <- c("1", "2")
  expect_equal(sort(Terms(ms)), terms)
  expect_equal(sort(Terms(mv)), terms)
  expect_equal(Docs(ms), docs)
  expect_equal(Docs(mv), docs)
  m <- matrix(c(0, 1, 0, 0, 0, 3, 2, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1), ncol = 2,
              dimnames = list("Terms" = terms, "Docs" = docs))
  expect_equal(as.matrix(ms[order(Terms(ms)), ]), m)
  expect_equal(as.matrix(mv), m)
})

test_that("construction with control arguments works", {
  vs <- VectorSource("one two two three three three")
  scorpus <- Corpus(vs)
  vcorpus <- VCorpus(vs)
  docs <- "1"
  ctrl <- list(dictionary = c("three", "two", "zero"))
  ms <- TermDocumentMatrix(scorpus, ctrl)
  mv <- TermDocumentMatrix(vcorpus, ctrl)
  m <- matrix(c(3, 2, 0),
              dimnames = list("Terms" = ctrl$dictionary, "Docs" = docs))
  expect_equal(as.matrix(ms[order(Terms(ms)), ]), m)
  expect_equal(as.matrix(mv), m)
})

test_that("zero matrix works", {
  vs <- VectorSource("one two three")
  scorpus <- Corpus(vs)
  vcorpus <- VCorpus(vs)
  ctrl <- list(dictionary = "four", wordLengths = c(1, Inf))
  ms <- TermDocumentMatrix(scorpus, ctrl)
  mv <- TermDocumentMatrix(vcorpus, ctrl)
  m <- matrix(0, dimnames = list("Terms" = ctrl$dictionary, "Docs" = "1"))
  expect_equal(as.matrix(ms), m)
  expect_equal(as.matrix(mv), m)
})

test_that("empty matrix works", {
  docs <- "1"
  ds <- DataframeSource(data.frame(doc_id = docs, text = NA))
  scorpus <- Corpus(ds)
  vcorpus <- VCorpus(ds)
  ms <- TermDocumentMatrix(scorpus)
  mv <- TermDocumentMatrix(vcorpus)
  m <- matrix(numeric(), dimnames = list("Terms" = character(), "Docs" = docs))
  expect_equal(as.matrix(ms), m)
  expect_equal(as.matrix(mv), m)
})
