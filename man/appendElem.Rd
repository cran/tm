\name{appendElem-methods}
\docType{methods}
\alias{appendElem}
\alias{appendElem-methods}
\alias{appendElem,TextDocCol,TextDocument-method}
\alias{appendElem,TextRepository,TextDocCol-method}
\title{Methods for Function appendElem in Package `textmin'}
\description{
Methods for function \code{appendElem} in Package `textmin'
}
\section{Methods}{
  \describe{
    \item{object = "TextDocCol", data = "TextDocument", meta = NULL}{Returns a text
      document collection where the \code{data} text document is added to
      the data slot of the \code{object} text document
      collection. Optionally meta data (\code{DMetaData}) can be added with
      \code{meta} to the document collection.}
    
    \item{object = "TextRepository", data = "TextDocCol", meta = NULL}{Returns a text
      repository where the \code{data} text document collection is added to
      the data slot of the \code{object} text repository. Optionally meta
      data can be added with \code{meta} to the repository.}
  }
}
\seealso{
\code{\link{DMetaData}}
\code{\link{RepoMetaData}}
}
\keyword{methods}
