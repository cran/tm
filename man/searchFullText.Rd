\name{searchFullText-methods}
\docType{methods}
\alias{searchFullText}
\alias{searchFullText-methods}
\alias{searchFullText,PlainTextDocument,character-method}
\title{Methods for Function searchFullText in Package `tm'}
\description{
  Methods for function \code{searchFullText} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "PlainTextDocument", pattern = "character"}{return
      \code{TRUE} if the regular expression \code{pattern} matches
      \code{object}'s text corpus.}
  }
}
\examples{
data("crude")
searchFullText(crude[[1]], "co[m]?pany")
}
\keyword{methods}
