\name{removeMeta-methods}
\docType{methods}
\alias{removeMeta}
\alias{removeMeta-methods}
\alias{removeMeta,TextDocCol-method}
\alias{removeMeta,TextRepository-method}
\title{Methods for Function removeMeta in Package `textmin'}
\description{
Methods for function \code{removeMeta} in Package `textmin'
}
\section{Methods}{
  \describe{
    \item{object = "TextDocCol", cmeta = NULL, dmeta = NULL}{Returns a text
      document collection where the \code{cmeta} is removed from the
      \code{CMetaData} and \code{dmeta} is removed from the \code{DMetaData}.}  
    \item{object = "TextRepository", cmeta = NULL, dmeta = NULL}{Returns a text
      repository where \code{cmeta} is removed from the metadata slot of the
      \code{object} text repository.}
  }
}
\seealso{
  \code{\link{DMetaData}}
  \code{\link{CMetaData}}
  \code{\link{RepoMetaData}}
}
\keyword{methods}
