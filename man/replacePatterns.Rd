\name{replacePatterns-methods}
\docType{methods}
\alias{replacePatterns}
\alias{replacePatterns-methods}
\alias{replacePatterns,PlainTextDocument,character,character-method}
\title{Methods for Function replacePatterns in Package `tm'}
\description{
Methods for function \code{replacePatterns} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "PlainTextDocument", patterns = "character", by = "character"}{Replaces all
      \code{patterns} (in POSIX 1003.2 extended regular expressions format, as used by gsub) which
      occur in \code{object} with \code{by} and return the object.}
  }
}
\examples{
data("crude")
crude[[1]]
replacePatterns(crude[[1]], "[[:space:]]((C|c)orp|copany)[[:space:]]", " company ")
}
\keyword{methods}
