\name{summary-methods}
\docType{methods}
\alias{summary-methods}
\alias{summary,Corpus-method}
\alias{summary,TextRepository-method}
\title{Methods for Function summary in Package `tm'}
\description{
Methods for function \code{summary} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "Corpus"}{Prints a description including information
      on available metadata tags.}
    \item{object = "TextRepository"}{Prints a description including information
      on available metadata tags.}
  }
}
\examples{
data("crude")
summary(crude)
}
\keyword{methods}
