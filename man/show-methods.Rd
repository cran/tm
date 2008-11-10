\name{show-methods}
\docType{methods}
\alias{show-methods}
\alias{show,PlainTextDocument-method}
\alias{show,Corpus-method}
\alias{show,TextRepository-method}
\title{Methods for Function show in Package `tm'}
\description{
  Methods for generic function \code{show} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "PlainTextDocument"}{Prints the text of a plain text
      document without any slots.}
    \item{object = "Corpus"}{Prints a short descriptive overview of the corpus.}
    \item{object = "TextRepository"}{Prints a short descriptive overview
      of the repository.}
  }
}
\examples{
data("crude")
show(crude[[1]])
}
\keyword{methods}
