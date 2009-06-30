\name{VCorpus-class}
\docType{class}
\alias{VCorpus-class}
\alias{CMetaData,VCorpus-method}
\alias{DMetaData,VCorpus-method}
\alias{DMetaData<-,VCorpus-method}
\title{Volatile Corpus}
\description{
  A class representing a volatile corpus.
}
\details{
  Volatile means that a corpus is fully kept in memory and thus all
  changes only affect the corresponding \R object.

  In contrast there is also a corpus implementation available providing
  a permanent semantics (see \code{\linkS4class{PCorpus}}).
}
\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{new("VCorpus",...)} or by calling the function
  \code{VCorpus}.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{list} containing the
      documents.}
    \item{\code{CMetaData}:}{Object of class \code{MetaDataNode} containing
      corpus specific meta data in form of tag-value pairs and information about
      children in form of a binary tree. This information is useful for
      reconstructing meta data after e.g. merging corpora.}
    \item{\code{DMetaData}:}{Object of class \code{data.frame} containing
      the document specific meta data for the collection. This data frame
      typically encompasses clustering or classification results which
      basically are metadata for documents but form an own entity (e.g.,
      with its name, the value range, etc.).}
  }
}
\section{Extends}{
Class \code{list}, directly.
}
\section{Methods}{
  \describe{
    \item{CMetaData}{\code{signature(object = "VCorpus")}: Returns the
      corpus specific metadata in form of a tag-value
      paired list.}
    \item{DMetaData}{\code{signature(object = "VCorpus")}: Returns the
      document specific metadata in form of a data frame.}
  }
}
\author{Ingo Feinerer}
\seealso{
  \code{\linkS4class{MetaDataNode}}
  \code{\link{VCorpus}}
  \code{\linkS4class{PCorpus}}
}
\keyword{classes}
