\name{removeSignature-methods}
\docType{methods}
\alias{removeSignature}
\alias{removeSignature-methods}
\alias{removeSignature,PlainTextDocument-method}
\title{Methods for Function removeSignature in Package `tm'}
\description{
Methods for function \code{removeSignature} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "PlainTextDocument"}{Removes signature lines from an
      e-mail message and return the object. Additional signature
      identification marks can be provided via the optional \code{marks}
      parameter in form of regular expression patterns.}
  }
}
\examples{
newsgroup <- system.file("texts", "newsgroup", package = "tm")
news <- TextDocCol(DirSource(newsgroup), readerControl = list(reader = readNewsgroup, language = "en_US", load = TRUE))
asPlain(news[[4]])
removeSignature(asPlain(news[[4]]))
asPlain(news[[5]])
removeSignature(asPlain(news[[5]]), marks = "^[+]-*[+]$")
}
\keyword{methods}
