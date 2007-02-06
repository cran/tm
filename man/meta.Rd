\name{meta-methods}
\docType{methods}
\alias{meta}
\alias{meta<-}
\alias{meta-methods}
\alias{meta,TextDocument-method}
\alias{meta<-,TextDocument-method}
\alias{meta,Corpus-method}
\alias{meta<-,Corpus-method}
\alias{meta,TextRepository-method}
\alias{meta<-,TextRepository-method}
\title{Methods for Function meta in Package `tm'}
\description{
Methods for function \code{meta} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "TextDocument"}{If no \code{tag} is given, this
      method pretty prints all \code{object}'s meta data. If \code{tag}
      is provided its value in the meta data is returned.}
    \item{object = "Corpus"}{If no \code{tag} is given, this
      method investigates the \code{type} argument. \code{type} must be
      either \code{indexed} (default) or \code{corpus}. Former is a
      shortcut for accessing document level meta data
      (\code{\link{DMetaData}}) stored at the collection level (because
      it forms an own entity, or for performance reasons, i.e., a form
      of indexing, hence the name indexed), the latter a shortcut for
      collection (corpus) specific meta data
      (\code{\link{CMetaData}}). Depending whether a tag is set or not,
      all or only the meta data identified by the tag is displayed or
      modified.}
    \item{object = "TextRepository"}{If no \code{tag} is given, this
      method pretty prints all \code{object}'s meta data. If \code{tag}
      is provided its value in the meta data is returned.}
  }
}
\examples{
data("crude")
meta(crude[[1]])
meta(crude[[1]], tag = "Topics")
meta(crude[[1]], tag = "Comment") <- "A short comment."
meta(crude[[1]])
meta(crude)
meta(crude, type = "corpus")
meta(crude, "labels") <- 21:40
meta(crude)
}
\keyword{methods}
