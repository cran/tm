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
    \item{Content}{\code{signature(object = "XMLTextDocument")}: Returns the
      text corpus, i.e., the actual XMLDocument in the data slot.}
    \item{Content<-}{\code{signature(object = "XMLTextDocument")}: Sets the
      text corpus, i.e., the actual XMLDocument in data slot.}
    \item{URI}{\code{signature(object = "XMLTextDocument")}: Returns the
      filename on disk.}
    \item{Cached}{\code{signature(object = "XMLTextDocument")}: Returns
      status information for loading on demand.}
    \item{Cached<-}{\code{signature(object = "XMLTextDocument")}: Sets
      status information for loading on demand.}
  }
}
\author{Ingo Feinerer}
\keyword{classes}
