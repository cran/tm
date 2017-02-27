\name{findMostFreqTerms}
\alias{findMostFreqTerms}
\alias{findMostFreqTerms.term_frequency}
\alias{findMostFreqTerms.DocumentTermMatrix}
\alias{findMostFreqTerms.TermDocumentMatrix}
\title{Find Most Frequent Terms}
\description{
  Find most frequent terms in a document-term or term-document matrix,
  or a vector of term frequencies.
}
\usage{
findMostFreqTerms(x, n = 6L, ...)
\S3method{findMostFreqTerms}{DocumentTermMatrix}(x, n = 6L, INDEX = NULL, ...)
\S3method{findMostFreqTerms}{TermDocumentMatrix}(x, n = 6L, INDEX = NULL, ...)
}
\arguments{
  \item{x}{A \code{\link{DocumentTermMatrix}} or
    \code{\link{TermDocumentMatrix}}, or a vector of term frequencies as
    obtained by \code{\link{termFreq}()}.}
  \item{n}{A single integer giving the maximal number of terms.}
  \item{INDEX}{an object specifying a grouping of documents for rollup,
    or \code{NULL} (default) in which case each document is considered
    individually.}
  \item{...}{arguments to be passed to or from methods.}
}
\value{
  For the document-term or term-document matrix methods, a list with the
  named frequencies of the up to \code{n} most frequent terms occurring
  in each document (group).  Otherwise, a single such vector of most
  frequent terms.
}
\details{
  Only terms with positive frequencies are included in the results.
}
\examples{
data("crude")

## Term frequencies:
tf <- termFreq(crude[[14L]])
findMostFreqTerms(tf)

## Document-term matrices:
dtm <- DocumentTermMatrix(crude)
## Most frequent terms for each document:
findMostFreqTerms(dtm)
## Most frequent terms for the first 10 the second 10 documents,
## respectively:
findMostFreqTerms(dtm, INDEX = rep(1 : 2, each = 10L))
}
