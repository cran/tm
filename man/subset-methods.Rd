\name{[-methods}
\docType{methods}
\alias{[-methods}
\alias{[<--methods}
\alias{[[-methods}
\alias{[[<--methods}
\alias{[,Corpus,ANY,ANY,ANY-method}
\alias{[,TermDocMatrix,ANY,ANY,ANY-method}
\alias{[<-,Corpus,ANY,ANY,ANY-method}
\alias{[[,Corpus,ANY,ANY-method}
\alias{[[<-,Corpus,ANY,ANY-method}
\title{Methods for Subset Functions in Package `tm'}
\description{
  Methods for subset functions \code{[}, \code{[<-}, \code{[[} and
  \code{[[<-} in package \pkg{tm}.
}
\arguments{
  \item{x = "Corpus"}{The text document collection to be subsetted
    or modified.}
  \item{x = "TermDocMatrix"}{The term-document matrix to be subsetted.}
}
\examples{
data("crude")
summary(crude[10:15])
crude[[1]]
}
\keyword{methods}
