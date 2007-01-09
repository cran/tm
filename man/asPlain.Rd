\name{asPlain-methods}
\docType{methods}
\alias{asPlain}
\alias{asPlain-methods}
\alias{asPlain,PlainTextDocument,ANY-method}
\alias{asPlain,XMLTextDocument,function-method}
\title{Methods for Function asPlain in Package `textmin'}
\description{
Methods for function \code{asPlain} in Package `textmin'
}
\section{Methods}{
  \describe{
    \item{object = "PlainTextDocument"}{Returns \code{object}.}
    \item{object = "XMLTextDocument", FUN}{Converts \code{object} to a
      \code{PlainTextDocument} by applying \code{FUN} on the XML root node
      and returns the new object.}
  }
}
\seealso{
  \code{\link{convertReut21578XMLPlain}}
  \code{\link{convertRCV1Plain}}
}
\keyword{methods}
