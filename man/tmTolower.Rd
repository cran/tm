\name{tmTolower-methods}
\docType{methods}
\alias{tmTolower}
\alias{tmTolower-methods}
\alias{tmTolower,PlainTextDocument-method}
\title{Methods for Function tmTolower in Package `tm'}
\description{
Methods for function \code{tmTolower} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "PlainTextDocument", ...}{Converts all
      words from \code{object} to lower case and return the object.}
  }
}
\examples{
data("crude")
crude[[1]]
tmTolower(crude[[1]])
}
\keyword{methods}
