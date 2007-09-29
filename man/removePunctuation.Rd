\name{removePunctuation-methods}
\docType{methods}
\alias{removePunctuation}
\alias{removePunctuation-methods}
\alias{removePunctuation,PlainTextDocument-method}
\title{Methods for Function removePunctuation in Package `tm'}
\description{
Methods for function \code{removePunctuation} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "PlainTextDocument"}{Removes all punctuation marks
      from \code{object} and return the object.}
  }
}
\examples{
data("crude")
crude[[1]]
removePunctuation(crude[[1]])
}
\keyword{methods}
