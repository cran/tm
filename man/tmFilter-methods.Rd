\name{tmFilter-methods}
\docType{methods}
\alias{tmFilter}
\alias{tmFilter-methods}
\alias{tmFilter,TextDocCol-method}
\title{Methods for Function tmFilter in Package `textmin'}
\description{
Methods for function \code{tmFilter} in Package `textmin'
}
\section{Methods}{
  \describe{
    \item{object = "TextDocCol", FUN  = sFilter, doclevel = FALSE}{If
      \code{doclevel} is set, the method applies \code{FUN} onto each element
      of \code{object} and returns the filtered \code{object}; otherwise
      \code{FUN} is applied to \code{object} itself, i.e., the
      filter is applied onto the \code{object}. Note that metadata is
      automatically passed to \code{FUN}. The function \code{FUN} must
      return a Boolean value.}
  }
}
\seealso{
  \code{\link{sFilter}}
  \code{\link{tmIndex}}
}
\keyword{methods}
