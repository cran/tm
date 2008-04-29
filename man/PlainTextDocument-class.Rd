\name{PlainTextDocument-class}
\docType{class}
\alias{PlainTextDocument-class}
\alias{Content}
\alias{Content<-}
\alias{URI}
\alias{Cached}
\alias{Cached<-}
\alias{Content,PlainTextDocument-method}
\alias{Content<-,PlainTextDocument-method}
\alias{URI,PlainTextDocument-method}
\alias{Cached,PlainTextDocument-method}
\alias{Cached<-,PlainTextDocument-method}
\title{Plain Text Document}
\description{
  A class representing a plain text document with additional information.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PlainTextDocument", ...)}.
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
Class \code{character} and \code{TextDocument}, directly.
}
\section{Methods}{
  \describe{
    \item{Content}{\code{signature(object = "PlainTextDocument")}: Returns the
      text corpus, i.e., the actual character data slot.}
    \item{Content<-}{\code{signature(object = "PlainTextDocument")}: Sets the
      text corpus, i.e., the actual character data slot.}
    \item{URI}{\code{signature(object = "PlainTextDocument")}: Returns the
      filename on disk.}
    \item{Cached}{\code{signature(object = "PlainTextDocument")}: Returns
      status information for loading on demand.}
    \item{Cached<-}{\code{signature(object = "PlainTextDocument")}: Sets
      status information for loading on demand.}
  }
}
\author{Ingo Feinerer}
\keyword{classes}
