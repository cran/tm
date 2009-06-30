\name{Corpus-class}
\docType{class}
\alias{Corpus-class}
\alias{CMetaData}
\alias{CMetaData,Corpus-method}
\alias{DMetaData}
\alias{DMetaData<-}
\title{Corpus}
\description{
  A (virtual) class representing a corpus.
}
\section{Objects from the Class}{
  Objects can be created by deriving and instantiating subclasses.
}
\section{Slots}{
  \describe{
    \item{\code{CMetaData}:}{Object of class \code{MetaDataNode} containing
      the corpus specific meta data in form of tag-value pairs and information about
      children in form of a binary tree. This information is useful for
      reconstructing meta data after e.g. merging corpora.}
    \item{\code{DMetaData}:}{Object of class \code{data.frame} containing
      the document specific meta data for the collection. This data frame
      typically encompasses clustering or classification results which
      basically are metadata for documents but form an own entity (e.g.,
      with its name, the value range, etc.).}
  }
}
\author{Ingo Feinerer}
\seealso{
  \code{\link{MetaDataNode-class}}
}
\keyword{classes}
