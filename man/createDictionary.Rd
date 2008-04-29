\name{createDictionary-methods}
\docType{methods}
\alias{createDictionary}
\alias{createDictionary-methods}
\alias{createDictionary,TermDocMatrix-method}
\title{Methods for Function createDictionary in Package `tm'}
\description{
Methods for function \code{createDictionary} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "TermDocMatrix"}{Creates a dictionary holding the
      column names of \code{object} as terms.
    }
  }
}
\examples{
data("crude")
tdm <- TermDocMatrix(crude)
(d <- createDictionary(tdm))
}
\keyword{methods}
