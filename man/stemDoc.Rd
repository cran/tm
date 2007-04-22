\name{stemDoc-methods}
\docType{methods}
\alias{stemDoc}
\alias{stemDoc-methods}
\alias{stemDoc,PlainTextDocument-method}
\title{Methods for Function stemDoc in Package `tm'}
\description{
Methods for function \code{stemDoc} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "PlainTextDocument"}{Stems the corpus from \code{object}
      with Porter's stemming algorithm.}
  }
}
\examples{
data("crude")
crude[[1]]
stemDoc(crude[[1]])
}
\keyword{methods}
