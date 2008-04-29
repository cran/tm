\name{findAssocs-methods}
\docType{methods}
\alias{findAssocs}
\alias{findAssocs-methods}
\alias{findAssocs,TermDocMatrix,character-method}
\alias{findAssocs,matrix,character-method}
\title{Methods for Function findAssocs in Package `tm'}
\description{
Methods for function \code{findAssocs} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "TermDocMatrix", term = "character", corlimit}{Finds those terms
      from \code{object} which correlate with \code{term} more than \code{corlimit}.}
    \item{object = "matrix", term = "character", corlimit}{Finds those terms
      from the correlation matrix \code{object} which correlate with \code{term} more than \code{corlimit}.}
  }
}
\examples{
data("crude")
tdm <- TermDocMatrix(crude)
findAssocs(tdm, "oil", 0.7)
}
\keyword{methods}
