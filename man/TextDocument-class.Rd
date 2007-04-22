\name{TextDocument-class}
\docType{class}
\alias{TextDocument-class}
\alias{Author}
\alias{Author<-}
\alias{DateTimeStamp}
\alias{DateTimeStamp<-}
\alias{Description}
\alias{Description<-}
\alias{ID}
\alias{ID<-}
\alias{Origin}
\alias{Origin<-}
\alias{Language}
\alias{Language<-}
\alias{Heading}
\alias{Heading<-}
\alias{LocalMetaData}
\alias{Author,TextDocument-method}
\alias{Author<-,TextDocument-method}
\alias{DateTimeStamp,TextDocument-method}
\alias{DateTimeStamp<-,TextDocument-method}
\alias{Description,TextDocument-method}
\alias{Description<-,TextDocument-method}
\alias{ID,TextDocument-method}
\alias{ID<-,TextDocument-method}
\alias{Origin,TextDocument-method}
\alias{Origin<-,TextDocument-method}
\alias{Heading,TextDocument-method}
\alias{Heading<-,TextDocument-method}
\alias{Language,TextDocument-method}
\alias{Language<-,TextDocument-method}
\alias{LocalMetaData,TextDocument-method}
\title{Text document}
\description{
  A class representing a text document with additional information.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("TextDocument", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{Author}:}{Object of class \code{character} containing
      the author names}
    \item{\code{DateTimeStamp}:}{Object of class \code{character} containing the
      date and time when the document was written}
    \item{\code{Description}:}{Object of class \code{character} containing
      additional text information}
    \item{\code{ID}:}{Object of class \code{integer} containing
      an identifier}
    \item{\code{Origin}:}{Object of class \code{character} containing
      information on the source and origin of the text}
    \item{\code{Heading}:}{Object of class \code{character} containing
      the title or a short heading}
    \item{\code{Language}:}{Object of class \code{character} containing
      the language of the text}
    \item{\code{LocalMetaData}:}{Object of class \code{list} containing
      the local metadata in form of tag-value pairs}
  }
}
\section{Methods}{
  \describe{
    \item{Author}{\code{signature(object = "TextDocument")}: returns the
      author names}
    \item{Author<-}{\code{signature(object = "TextDocument")}: sets the
      author names}
    \item{DateTimeStamp}{\code{signature(object = "TextDocument")}: returns the
      date and time when the document was written}
    \item{DateTimeStamp<-}{\code{signature(object = "TextDocument")}: sets the
      date and time when the document was written}
    \item{Description}{\code{signature(object = "TextDocument")}: returns
      additional text information}
    \item{Description<-}{\code{signature(object = "TextDocument")}: sets
      additional text information}
    \item{ID}{\code{signature(object = "TextDocument")}: returns the
      identifier}
    \item{ID<-}{\code{signature(object = "TextDocument")}: sets the
      identifier}
    \item{Origin}{\code{signature(object = "TextDocument")}: returns
      information on the source and origin of the text}
    \item{Origin<-}{\code{signature(object = "TextDocument")}: sets
      information on the source and origin of the text}
    \item{Heading}{\code{signature(object = "TextDocument")}: returns
      the title or a short heading}
    \item{Heading<-}{\code{signature(object = "TextDocument")}: sets
      the title or a short heading}
    \item{Language}{\code{signature(object = "TextDocument")}: returns
      the text language}
    \item{Language<-}{\code{signature(object = "TextDocument")}: sets
      the text language}
    \item{LocalMetaData}{\code{signature(object = "TextDocument")}: returns
      the local metadata in form of a tag-value paired list}
  }
}
\author{Ingo Feinerer}
\keyword{classes}
