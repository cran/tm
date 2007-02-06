\name{XMLTextDocument-class}
\docType{class}
\alias{XMLTextDocument-class}
\alias{Content,XMLTextDocument-method}
\alias{Content<-,XMLTextDocument-method}
\alias{URI,XMLTextDocument-method}
\alias{Cached,XMLTextDocument-method}
\alias{Cached<-,XMLTextDocument-method}
\title{Text document}
\description{
  A class representing an XML text document with additional
  information. The XML document itself is represented by an
  \code{XMLDocument} from the \pkg{XML} package.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("XMLTextDocument", ...)}.
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
    \item{Content}{\code{signature(object = "XMLTextDocument")}: returns the
      text corpus, i.e., the actual XMLDocument in the data slot}
    \item{Content<-}{\code{signature(object = "XMLTextDocument")}: sets the
      text corpus, i.e., the actual XMLDocument in data slot}
    \item{URI}{\code{signature(object = "XMLTextDocument")}: returns the
      filename on disk}
    \item{Cached}{\code{signature(object = "XMLTextDocument")}: returns
      status information for loading on demand}
    \item{Cached<-}{\code{signature(object = "XMLTextDocument")}: sets
      status information for loading on demand}
  }
}
\author{Ingo Feinerer}
\keyword{classes}
