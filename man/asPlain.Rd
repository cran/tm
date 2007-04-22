\name{asPlain-methods}
\docType{methods}
\alias{asPlain}
\alias{asPlain-methods}
\alias{asPlain,NewsgroupDocument,ANY-method}
\alias{asPlain,PlainTextDocument,ANY-method}
\alias{asPlain,XMLTextDocument,function-method}
\title{Methods for Function asPlain in Package `tm'}
\description{
Methods for function \code{asPlain} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "NewsgroupDocument"}{Returns \code{object} to a plain
    text document.}
    \item{object = "PlainTextDocument"}{Returns \code{object}.}
    \item{object = "XMLTextDocument", FUN}{Converts \code{object} to a
      \code{PlainTextDocument} by applying \code{FUN} on the XML root node
      and returns the new object.}
  }
}
\seealso{
  \code{\link{convertReut21578XMLPlain}}
  \code{\link{convertRCV1Plain}}
}
\examples{
reut21578 <- system.file("texts", "reut21578", package = "tm")
reut21578TDC <- TextDocCol(DirSource(reut21578), readerControl = list(reader = readReut21578XML, language = "en_US", load = TRUE))
reut21578TDC[[1]]
asPlain(reut21578TDC[[1]], convertReut21578XMLPlain)
}
\keyword{methods}
