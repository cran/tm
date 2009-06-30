\name{asPlain-methods}
\docType{methods}
\alias{asPlain}
\alias{asPlain-methods}
\alias{asPlain,NewsgroupDocument-method}
\alias{asPlain,PlainTextDocument-method}
\alias{asPlain,XMLTextDocument-method}
\alias{asPlain,Reuters21578Document-method}
\alias{asPlain,RCV1Document-method}
\alias{asPlain,StructuredTextDocument-method}
\title{Methods for Function asPlain in Package `tm'}
\description{
Methods for function \code{asPlain} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "NewsgroupDocument"}{Converts \code{object} to a plain
    text document.}
    \item{object = "PlainTextDocument"}{Returns \code{object}.}
    \item{object = "XMLTextDocument", FUN}{Converts \code{object} to a
      \code{PlainTextDocument} by applying \code{FUN} on the XML root node
      and returns the new object.}
    \item{object = "Reuters21578Document"}{Converts \code{object} to a
      \code{PlainTextDocument}.}
    \item{object = "RCV1Document"}{Converts \code{object} to a
      \code{PlainTextDocument}.}
    \item{object = "StructuredTextDocument"}{Converts \code{object} to a
      plain text document.}
  }
}
\seealso{
  \code{\link{convertReut21578XMLPlain}}
  \code{\link{convertRCV1Plain}}

  Use \code{\link{getTransformations}} to list available transformation (mapping) functions.
}
\examples{
reut21578 <- system.file("texts", "reut21578", package = "tm")
reut21578TDC <- Corpus(DirSource(reut21578), readerControl = list(reader = readReut21578XML))
reut21578TDC[[1]]
asPlain(reut21578TDC[[1]])
}
\keyword{methods}
