\name{\%IN\%-methods}
\alias{\%IN\%}
\docType{methods}
\alias{\%IN\%-methods}
\alias{\%IN\%,TextDocument,VCorpus-method}
\alias{\%IN\%,TextDocument,PCorpus-method}
\title{Methods for Function \%IN\% in Package `tm'}
\description{
  Methods for function \code{\%IN\%} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{x = "TextDocument", y = "Corpus"}{Returns \code{TRUE} if
      \code{x} occurs in \code{y}.}
  }
}
\examples{
data("crude")
crude[[1]] \%IN\% crude
}
\keyword{methods}
