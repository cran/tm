\name{findFreqTerms-methods}
\docType{methods}
\alias{findFreqTerms}
\alias{findFreqTerms-methods}
\alias{findFreqTerms,TermDocMatrix-method}
\title{Methods for Function findFreqTerms in Package `tm'}
\description{
Methods for function \code{findFreqTerms} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "TermDocMatrix", lowfreq = 0, highfreq = Inf}{Return
      those terms from \code{object} which occur more or equal often
      than \code{lowfreq} times and less or equal often than
      \code{highfreq} times. This method works for all numeric
      weightings but is probably most meaningful for the standard term
      frequency (\code{tf}) weighting of \code{object}.
    }
  }
}
\examples{
data("crude")
tdm <- TermDocMatrix(crude)
findFreqTerms(tdm, 2, 3)
}
\keyword{methods}
