\name{StructuredTextDocument-class}
\docType{class}
\alias{StructuredTextDocument-class}
\alias{Content,StructuredTextDocument-method}
\alias{Content<-,StructuredTextDocument-method}
\alias{URI,StructuredTextDocument-method}
\alias{Cached,StructuredTextDocument-method}
\alias{Cached<-,StructuredTextDocument-method}
\title{Text document}
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
      the path and filename holding the data physically on disk}
    \item{\code{Cached}:}{Object of class \code{numeric} containing the
      status whether the file was already loaded in memory. Zero indicates
      not loaded, a value greater zero indicates in memory.}
  }
}
\section{Extends}{
Class \code{list} and \code{TextDocument}, directly.
}
\section{Methods}{
  \describe{
    \item{Content}{\code{signature(object = "StructuredTextDocument")}: returns the
      text corpus, i.e., the actual character data slot}
    \item{Content<-}{\code{signature(object = "StructuredTextDocument")}: sets the
      text corpus, i.e., the actual character data slot}
    \item{URI}{\code{signature(object = "StructuredTextDocument")}: returns the
      filename on disk}
    \item{Cached}{\code{signature(object = "StructuredTextDocument")}: returns
      status information for loading on demand}
    \item{Cached<-}{\code{signature(object = "StructuredTextDocument")}: sets
      status information for loading on demand}
  }
}
\author{Ingo Feinerer}
\keyword{classes}
