\name{removeMeta-methods}
\docType{methods}
\alias{removeMeta}
\alias{removeMeta-methods}
\alias{removeMeta,Corpus-method}
\alias{removeMeta,TextRepository-method}
\title{Methods for Function removeMeta in Package `tm'}
\description{
Methods for function \code{removeMeta} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "Corpus", cname = NULL, dname = NULL}{Returns a text
      document collection where the \code{cname} is removed from the
      \code{CMetaData} and \code{dname} is removed from the \code{DMetaData}.}  
    \item{object = "TextRepository", cname = NULL, dname = NULL}{Returns a text
      repository where \code{cname} is removed from the metadata slot of the
      \code{object} text repository.}
  }
}
\seealso{
  \code{\link{DMetaData}}
  \code{\link{CMetaData}}
  \code{\link{RepoMetaData}}
}
\examples{
data("crude")
CMetaData(crude)
DMetaData(crude)
tdcl <- appendMeta(crude, cmeta = list(created = date()), dmeta = list(numbers = 1:20))
CMetaData(tdcl)
DMetaData(tdcl)
tdcl <- removeMeta(crude, cname = "created", dname = "numbers")
CMetaData(tdcl)
DMetaData(tdcl)
}
\keyword{methods}
