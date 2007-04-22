\name{length-methods}
\docType{methods}
\alias{length}
\alias{length-methods}
\alias{length,TextDocCol-method}
\alias{length,TextRepository-method}
\title{Methods for Function length in Package `tm'}
\description{
Methods for function \code{length} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{x = "TextDocCol"}{Returns the number of text documents in \code{x}.}
    \item{x = "TextRepository"}{Returns the number of text document
      collections in \code{x}.}
  }
}
\examples{
data("crude")
length(crude)
length(TextRepository(crude))
}
\keyword{methods}
