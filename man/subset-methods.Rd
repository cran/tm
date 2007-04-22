\name{[-methods}
\docType{methods}
\alias{[-methods}
\alias{[<--methods}
\alias{[[-methods}
\alias{[[<--methods}
\alias{[,TextDocCol,ANY,ANY,ANY-method}
\alias{[<-,TextDocCol,ANY,ANY,ANY-method}
\alias{[[,TextDocCol,ANY,ANY-method}
\alias{[[<-,TextDocCol,ANY,ANY-method}
\title{Methods for Subset Functions in Package `tm'}
\description{
  Methods for subset functions \code{[}, \code{[<-}, \code{[[} and
  \code{[[<-} in package \pkg{tm}..
}
\arguments{
  \item{x = "TextDocCol"}{the text document collection to be subsetted
    or modified.}
}
\examples{
data("crude")
summary(crude[10:15])
crude[[1]]
}
\keyword{methods}
