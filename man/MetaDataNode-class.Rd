\name{MetaDataNode-class}
\docType{class}
\alias{MetaDataNode-class}
\title{Metadata Node}
\description{
  A class representing a node in the metadata tree of a text document
  collection.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{new("MetaDataNode",...)}.
}
\section{Slots}{
  \describe{
    \item{\code{NodeID}:}{Object of class \code{"numeric"} containing
      a unique identification number for the node.}
    \item{\code{MetaData}:}{Object of class \code{"list"} containing
      meta data at node level.}
    \item{\code{children}:}{Object of class \code{"list"} containing
      either zero, one or two \code{MetaDataNode}s resulting in a binary
      tree.}
  }
}
\author{Ingo Feinerer}
\keyword{classes}
