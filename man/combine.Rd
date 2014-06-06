\name{tm_combine}
\alias{c.VCorpus}
\alias{c.TextDocument}
\alias{c.TermDocumentMatrix}
\alias{c.term_frequency}
\title{Combine Corpora, Documents, Term-Document Matrices, and Term Frequency Vectors}
\description{
  Combine several corpora into a single one, combine multiple
  documents into a corpus, combine multiple term-document matrices
  into a single one, or combine multiple term frequency vectors into a
  single term-document matrix.
}
\usage{
\method{c}{VCorpus}(\dots, recursive = FALSE)
\method{c}{TextDocument}(\dots, recursive = FALSE)
\method{c}{TermDocumentMatrix}(\dots, recursive = FALSE)
\method{c}{term_frequency}(\dots, recursive = FALSE)
}
\arguments{
  \item{\dots}{Corpora, text documents, term-document matrices, or term
    frequency vectors.}
  \item{recursive}{Not used.}
}
\seealso{
  \code{\link{VCorpus}}, \code{\link{TextDocument}},
  \code{\link{TermDocumentMatrix}}, and \code{\link{termFreq}}.
}
\examples{
data("acq")
data("crude")
meta(acq, "comment", type = "corpus") <- "Acquisitions"
meta(crude, "comment", type = "corpus") <- "Crude oil"
meta(acq, "acqLabels") <- 1:50
meta(acq, "jointLabels") <- 1:50
meta(crude, "crudeLabels") <- letters[1:20]
meta(crude, "jointLabels") <- 1:20
c(acq, crude)
meta(c(acq, crude), type = "corpus")
meta(c(acq, crude))
c(acq[[30]], crude[[10]])
c(TermDocumentMatrix(acq), TermDocumentMatrix(crude))
}
