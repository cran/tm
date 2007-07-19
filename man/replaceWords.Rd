\name{replaceWords-methods}
\docType{methods}
\alias{replaceWords}
\alias{replaceWords-methods}
\alias{replaceWords,PlainTextDocument,character,character-method}
\title{Methods for Function replaceWords in Package `tm'}
\description{
Methods for function \code{replaceWords} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "PlainTextDocument", words = "character", by = "character"}{Replaces all
      \code{words} which occur in \code{object} with \code{by} and return the object.}
  }
}
\examples{
data("crude")
crude[[1]]
replaceWords(crude[[1]], c("Corp", "copany"), "company")
}
\keyword{methods}
