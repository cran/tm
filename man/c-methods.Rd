\name{c-methods}
\docType{methods}
\alias{c-methods}
\alias{c,Corpus-method}
\alias{c,TextDocument-method}
\title{Methods for Function c in Package `tm'}
\description{
Methods for function \code{c} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{x = "Corpus"}{concatenates several text document collections
      to a single one.}
    \item{x = "TextDocument"}{concatenates several text documents
      to a single text document collection.}
  }
}
\examples{
data("acq")
data("crude")
summary(c(acq,crude))
summary(c(acq[[30]],crude[[10]]))
}
\keyword{methods}
