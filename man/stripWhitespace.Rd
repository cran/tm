\name{stripWhitespace-methods}
\docType{methods}
\alias{stripWhitespace}
\alias{stripWhitespace-methods}
\alias{stripWhitespace,PlainTextDocument-method}
\title{Methods for Function stripWhitespace in Package `tm'}
\description{
Methods for function \code{stripWhitespace} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "PlainTextDocument", ...}{Strips unnecessary
      whitespace from \code{object} and return the object.}
  }
}
\examples{
data("crude")
crude[[1]]
stripWhitespace(crude[[1]])
}
\keyword{methods}
