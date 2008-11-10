\name{c-methods}
\docType{methods}
\alias{c-methods}
\alias{c,Corpus-method}
\alias{c,TextDocument-method}
\title{Methods for Function c in Package `tm'}
\description{
Methods for generic function \code{c} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{x = "Corpus"}{Concatenates several text document collections
      to a single one.}
    \item{x = "TextDocument"}{Concatenates several text documents
      to a single text document collection.}
  }
}
\details{
  Note that meta data from single items (corpora or documents) is
  preserved during concatenation and intelligently merged into the newly
  created corpus. Although we use a sophisticated merging strategy
  (by using a binary tree for corpus specific meta data and by joining document
  level specific meta data in data frames) you should check the newly
  created meta data for consistency when merging corpora with (partly)
  identical meta data. However, in most cases the meta data merging
  strategy will produce perfectly combined and arranged meta data
  structures.
}
\examples{
data("acq")
data("crude")
summary(c(acq,crude))
summary(c(acq[[30]],crude[[10]]))
}
\keyword{methods}
