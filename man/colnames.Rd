\name{colnames-methods}
\docType{methods}
\alias{colnames-methods}
\alias{colnames,TermDocMatrix-method}
\title{Methods for Function colnames in Package `tm'}
\description{
Methods for function \code{colnames} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{x = "TermDocMatrix"}{Retrieve the column names of the
      term-document matrix.}
  }
}
\examples{
data("crude")
tdm <- TermDocMatrix(crude)
colnames(tdm)
}
\keyword{methods}
