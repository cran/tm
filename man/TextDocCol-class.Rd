\name{Corpus-class}
\docType{class}
\alias{Corpus-class}
\alias{CMetaData}
\alias{DBControl}
\alias{DMetaData}
\alias{DMetaData<-}
\alias{CMetaData,Corpus-method}
\alias{DBControl,Corpus-method}
\alias{DMetaData,Corpus-method}
\alias{DMetaData<-,Corpus-method}
\title{Corpus}
\description{
  A class representing a collection of text documents (denoted as corpus
  in linguistics).
}
\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{new("Corpus",...)} or by calling the function
  \code{Corpus}.
}
\section{Slots}{
  \describe{
    \item{\code{CMetaData}:}{Object of class \code{MetaDataNode} containing
      the Document Collection specific MetaData for the collection in form of
      tag-value pairs and information about children in form of a binary
      tree. This information is useful for reconstructing meta data
      after e.g. merging document collections.}
    \item{\code{DMetaData}:}{Object of class \code{data.frame} containing
      the Document specific MetaData for the collection. This dataframe
      typically encompasses clustering or classification results which
      basically are metadata for documents but form an own entity (e.g.,
      with its name, the value range, etc.).}
    \item{\code{DBControl}:}{Object of class \code{list} with three
      named components: \code{useDb} indicates whether database support
      is activated, \code{dbName} holds the path to the database
      storage, and \code{dbType} stores the database type.}
  }
}
\section{Extends}{
Class \code{list}, directly.
}
\section{Methods}{
  \describe{
    \item{CMetaData}{\code{signature(object = "Corpus")}: Returns the
      document collection specific metadata in form of a tag-value
      paired list.}
    \item{DMetaData}{\code{signature(object = "Corpus")}: Returns the
      document specific metadata in form of a data frame.}
    \item{DBControl}{\code{signature(object = "Corpus")}: Returns
      the database configuration settings.}
  }
}
\author{Ingo Feinerer}
\seealso{
  \code{\link{MetaDataNode-class}}
  \code{\link{Corpus}}
}
\keyword{classes}
