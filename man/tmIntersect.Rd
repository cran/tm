\name{tmIntersect-methods}
\docType{methods}
\alias{tmIntersect}
\alias{tmIntersect-methods}
\alias{tmIntersect,PlainTextDocument,character-method}
\title{Methods for Function tmIntersect in Package `tm'}
\description{
  Methods for function \code{tmIntersect} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "PlainTextDocument", words = "character"}{return
      \code{TRUE} if \code{words} appear in \code{object}'s text corpus.}
  }
}
\examples{
data("crude")
tmIntersect(crude[[1]], c("crude", "oil"))
tmIntersect(crude[[1]], "acquisition")
}
\keyword{methods}
