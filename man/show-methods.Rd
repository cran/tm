\name{show-methods}
\docType{methods}
\alias{show-methods}
\alias{show,PlainTextDocument-method}
\alias{show,TextDocCol-method}
\alias{show,TextRepository-method}
\title{Methods for Function show in Package `tm'}
\description{
  Methods for function \code{show} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "PlainTextDocument"}{Prints the text of a plain text
      document without all slots.}
    \item{object = "TextDocCol"}{Prints some short information on a text
      document collection.}
    \item{object = "TextRepository"}{Prints some short information on a text
      repository.}
  }
}
\examples{
data("crude")
show(crude[[1]])
}
\keyword{methods}
