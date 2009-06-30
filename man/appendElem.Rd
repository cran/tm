\name{appendElem-methods}
\docType{methods}
\alias{appendElem}
\alias{appendElem-methods}
\alias{appendElem,Corpus,TextDocument-method}
\alias{appendElem,TextRepository,Corpus-method}
\title{Methods for Function appendElem in Package `tm'}
\description{
Methods for function \code{appendElem} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "Corpus", data = "TextDocument", meta = NULL}{Returns a text
      document collection where the \code{data} text document is added to
      the data slot of the \code{object} text document
      collection. Optionally meta data (\code{DMetaData}) can be added with
      \code{meta} to the document collection.}
    
    \item{object = "TextRepository", data = "Corpus", meta = NULL}{Returns a text
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
