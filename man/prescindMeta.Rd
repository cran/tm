\name{prescindMeta-methods}
\docType{methods}
\alias{prescindMeta}
\alias{prescindMeta-methods}
\alias{prescindMeta,Corpus,character-method}
\title{Methods for Function prescindMetadata in Package `tm'}
\description{
Methods for function \code{prescindMeta} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "Corpus", meta = "character"}{Copies the
      \code{meta} data from document slots or (local) document meta data
      of all documents in \code{object} to the (global) \code{DMetaData}
      data frame of \code{object} and returns the whole collection.}
  }
}
\value{
  A \code{\link{Corpus}} constructed from \code{object} with shifted up
  \code{meta} data.
}
\seealso{
  \code{\link{DMetaData}}
}
\examples{
data("crude")
DMetaData(crude)
DMetaData(prescindMeta(crude, c("ID", "Heading")))
}
\keyword{methods}
