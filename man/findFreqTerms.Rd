\name{findFreqTerms}
\alias{findFreqTerms}
\title{Find Frequent Terms}
\description{
  Find frequent terms in a document-term or term-document matrix.
}
\usage{
findFreqTerms(x, lowfreq = 0, highfreq = Inf)
}
\arguments{
  \item{x}{A \code{\link{DocumentTermMatrix}} or
    \code{\link{TermDocumentMatrix}}.}
  \item{lowfreq}{A numeric for the lower frequency bound.}
  \item{highfreq}{A numeric for the upper frequency bound.}
}
\value{
  A character vector of terms in \code{x} which occur more or equal often
  than \code{lowfreq} times and less or equal often than \code{highfreq}
  times.
}
\details{This method works for all numeric weightings but is probably
  most meaningful for the standard term frequency (\code{tf}) weighting
  of \code{x}.}
\examples{
data("crude")
tdm <- TermDocumentMatrix(crude)
findFreqTerms(tdm, 2, 3)
}
