\name{ncol-methods}
\docType{methods}
\alias{ncol-methods}
\alias{ncol,TermDocMatrix-method}
\title{Methods for Function ncol in Package `tm'}
\description{
Methods for function \code{ncol} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{x = "TermDocMatrix"}{Retrieve the number of columns of the
      term-document matrix.}
  }
}
\examples{
data("crude")
tdm <- TermDocMatrix(crude)
ncol(tdm)
}
\keyword{methods}
