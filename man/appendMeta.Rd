\name{appendMeta-methods}
\docType{methods}
\alias{appendMeta}
\alias{appendMeta-methods}
\alias{appendMeta,TextDocCol-method}
\alias{appendMeta,TextRepository-method}
\title{Methods for Function appendMeta in Package `textmin'}
\description{
Methods for function \code{appendMeta} in Package `textmin'
}
\section{Methods}{
  \describe{
    \item{object = "TextDocCol", cmeta = NULL, dmeta = NULL}{Returns a text
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
\keyword{methods}
