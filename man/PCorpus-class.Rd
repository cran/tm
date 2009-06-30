\name{PCorpus-class}
\docType{class}
\alias{PCorpus-class}
\alias{CMetaData,PCorpus-method}
\alias{DBControl}
\alias{DBControl,PCorpus-method}
\alias{DMetaData,PCorpus-method}
\alias{DMetaData<-,PCorpus-method}
\title{Permanent Corpus}
\description{
  A class representing a corpus with underlying permanent storage.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{new("PCorpus",...)} or by calling the function
  \code{PCorpus}.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{list} holding keys (the
      document IDs) in order to access the underlying database entries.}
    \item{\code{CMetaData}:}{Object of class \code{MetaDataNode} containing
      the corpus specific meta data in form of tag-value pairs and information about
      children in form of a binary tree. This information is useful for
      reconstructing meta data after e.g. merging corpora.}
    \item{\code{DMetaData}:}{Object of class \code{data.frame} containing
      the document specific meta data for the collection. This data frame
      typically encompasses clustering or classification results which
      basically are metadata for documents but form an own entity (e.g.,
      with its name, the value range, etc.).}
    \item{\code{DBControl}:}{Object of class \code{list} with two
      named components: \code{dbName} holds the path to the permanent database
      storage, and \code{dbType} stores the database type.}
  }
}
\section{Extends}{
Class \code{list}, directly.
}
\section{Methods}{
  \describe{
    \item{CMetaData}{\code{signature(object = "PCorpus")}: Returns the
      corpus specific metadata in form of a tag-value
      paired list.}
    \item{DMetaData}{\code{signature(object = "PCorpus")}: Returns the
      document specific metadata in form of a data frame.}
    \item{DBControl}{\code{signature(object = "PCorpus")}: Returns
      the database configuration settings.}
  }
}
\author{Ingo Feinerer}
\seealso{
  \code{\linkS4class{MetaDataNode}}
  \code{\link{PCorpus}}
  \code{\linkS4class{VCorpus}}
}
\keyword{classes}
