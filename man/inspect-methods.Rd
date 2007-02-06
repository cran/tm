\name{inspect-methods}
\docType{methods}
\alias{inspect}
\alias{inspect-methods}
\alias{inspect,Corpus-method}
\title{Methods for Function inspect in Package `tm'}
\description{
  Methods for function \code{inspect} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "Corpus"}{Displays detailed information for
      \code{object}, i.e., it prints all text documents and metadata.}
  }
}
\examples{
data("crude")
inspect(crude[1:3])
}
\keyword{methods}
