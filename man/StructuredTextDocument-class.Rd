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
    \item{\code{.Data}:}{Object of class \code{character} containing the
      content.}
    \item{\code{Author}:}{Object of class \code{character} containing
      the author names.}
    \item{\code{DateTimeStamp}:}{Object of class \code{character} containing the
      date and time when the document was written.}
    \item{\code{Description}:}{Object of class \code{character} containing
      additional text information.}
    \item{\code{ID}:}{Object of class \code{integer} containing
      an identifier.}
    \item{\code{Origin}:}{Object of class \code{character} containing
      information on the source and origin of the text.}
    \item{\code{Heading}:}{Object of class \code{character} containing
      the title or a short heading.}
    \item{\code{Language}:}{Object of class \code{character} containing
      the language of the text.}
    \item{\code{LocalMetaData}:}{Object of class \code{list} containing
      the local meta data in form of tag-value pairs.}
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
