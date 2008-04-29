\name{rownames-methods}
\docType{methods}
\alias{rownames-methods}
\alias{rownames,TermDocMatrix-method}
\title{Methods for Function rownames in Package `tm'}
\description{
Methods for function \code{rownames} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{x = "TermDocMatrix"}{Retrieve the row names of the
      term-document matrix \code{x}.}
  }
}
\examples{
data("crude")
tdm <- TermDocMatrix(crude)
rownames(tdm)
}
\keyword{methods}
