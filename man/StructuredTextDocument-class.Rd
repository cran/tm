\name{StructuredTextDocument-class}
\docType{class}
\alias{StructuredTextDocument-class}
\alias{Content,StructuredTextDocument-method}
\alias{Content<-,StructuredTextDocument-method}
\alias{URI,StructuredTextDocument-method}
\alias{Cached,StructuredTextDocument-method}
\alias{Cached<-,StructuredTextDocument-method}
\title{Structured Text Document}
\description{
  A class representing a structured text with additional
  information.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("StructuredTextDocument", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{URI}:}{Object of class \code{character} containing
      the path and filename holding the data physically on disk.}
    \item{\code{Cached}:}{Object of class \code{logical} containing the
      status whether the file was already loaded into memory.}
  }
}
\section{Extends}{
Class \code{list} and \code{TextDocument}, directly.
}
\section{Methods}{
  \describe{
    \item{Content}{\code{signature(object = "StructuredTextDocument")}: Returns the
      text corpus, i.e., the actual character data slot.}
    \item{Content<-}{\code{signature(object = "StructuredTextDocument")}: Sets the
      text corpus, i.e., the actual character data slot.}
    \item{URI}{\code{signature(object = "StructuredTextDocument")}: Returns the
      filename on disk.}
    \item{Cached}{\code{signature(object = "StructuredTextDocument")}: Returns
      status information for loading on demand.}
    \item{Cached<-}{\code{signature(object = "StructuredTextDocument")}: Sets
      status information for loading on demand.}
  }
}
\author{Ingo Feinerer}
\keyword{classes}
