\name{appendMeta-methods}
\docType{methods}
\alias{appendMeta}
\alias{appendMeta-methods}
\alias{appendMeta,Corpus-method}
\alias{appendMeta,TextRepository-method}
\title{Methods for Function appendMeta in Package `tm'}
\description{
Methods for function \code{appendMeta} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "Corpus", cmeta = NULL, dmeta = NULL}{Returns a text
      document collection where the \code{cmeta} is added to the
      \code{CMetaData} and \code{dmeta} is added to the \code{DMetaData}.}  
    \item{object = "TextRepository", cmeta = NULL, dmeta = NULL}{Returns a text
      repository where \code{cmeta} is added to the metadata slot of the
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
summary(crude)
CMetaData(crude)
DMetaData(crude)
tdcl <- appendMeta(crude, cmeta = list("changed" = date()), dmeta = list("number" = 1:20))
summary(tdcl)
CMetaData(tdcl)
DMetaData(tdcl)
}
\keyword{methods}
