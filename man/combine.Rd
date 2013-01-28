\name{tm_combine}
\alias{c.Corpus}
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
\method{c}{Corpus}(\dots, recursive = FALSE)
\method{c}{TextDocument}(\dots, recursive = FALSE)
\method{c}{TermDocumentMatrix}(\dots, recursive = FALSE)
\method{c}{term_frequency}(\dots, recursive = FALSE)
}
\arguments{
  \item{\dots}{Corpora, text documents, term-document matrices, or term
    frequency vectors.}
  \item{recursive}{Logical. If \code{recursive = TRUE} existing corpus
    meta data is also merged, otherwise discarded.}
}
\details{
  If \code{recursive = TRUE}, meta data from input objects (corpora or
  documents) is preserved during concatenation and intelligently merged
  into the newly created corpus. Although we use a sophisticated merging
  strategy (by using a binary tree for corpus specific meta data and by
  joining document level specific meta data in data frames) you should
  check the newly created meta data for consistency when merging corpora
  with (partly) identical meta data. However, in most cases the meta
  data merging strategy will produce validly combined and arranged meta
  data structures.
}
\seealso{
  \code{\link{Corpus}}, \code{\link{TextDocument}},
  \code{\link{TermDocumentMatrix}}, and \code{\link{termFreq}}.
}
\examples{
data("acq")
data("crude")
summary(c(acq, crude))
summary(c(acq[[30]], crude[[10]]))
c(TermDocumentMatrix(acq), TermDocumentMatrix(crude))
}
