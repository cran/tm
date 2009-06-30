\name{NewsgroupDocument-class}
\docType{class}
\alias{NewsgroupDocument-class}
\alias{Content,NewsgroupDocument-method}
\alias{Content<-,NewsgroupDocument-method}
\alias{URI,NewsgroupDocument-method}
\alias{Cached,NewsgroupDocument-method}
\alias{Cached<-,NewsgroupDocument-method}
\title{Newsgroup Text Document}
\description{
  A class representing a newsgroup document with additional
  information. The newsgroup documents must be formatted according to
  the Newsgroup dataset from the \acronym{UCI} \acronym{KDD} archive.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("NewsgroupDocument", ...)}.
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
    \item{\code{Newsgroup}:}{Object of class \code{character} containing
      the newsgroups where the document has been posted.}
  }
}
\section{Extends}{
Class \code{character} and \code{TextDocument}, directly.
}
\section{Methods}{
  \describe{
    \item{Content}{\code{signature(object = "NewsgroupDocument")}: Returns the
      text corpus, i.e., the actual character data slot.}
    \item{Content<-}{\code{signature(object = "NewsgroupDocument")}: Sets the
      text corpus, i.e., the actual character data slot.}
  }
}
\references{\url{http://kdd.ics.uci.edu/databases/20newsgroups/20newsgroups.html}}
\author{Ingo Feinerer}
\keyword{classes}
