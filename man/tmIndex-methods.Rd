\name{tmIndex-methods}
\docType{methods}
\alias{tmIndex}
\alias{tmIndex-methods}
\alias{tmIndex,TextDocCol-method}
\title{Methods for Function tmIndex in Package `textmin'}
\description{
Methods for function \code{tmIndex} in Package `textmin'
}
\section{Methods}{
  \describe{
    \item{object = "TextDocCol", FUN  = sFilter, doclevel = FALSE}{If
      \code{doclevel} is set, the method applies \code{FUN} onto each element
      of \code{object} and returns the indices; otherwise
      \code{FUN} is applied to \code{object} itself. Note that metadata is
      automatically passed to \code{FUN}. The function \code{FUN} must
      return a Boolean value.}
  }
}
\seealso{
  \code{\link{sFilter}}
  \code{\link{tmFilter}}
}
\keyword{methods}
