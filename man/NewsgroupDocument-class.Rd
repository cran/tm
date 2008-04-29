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
    \item{\code{Newsgroup}:}{Object of class \code{character} containing
      the newsgroups where the document has been posted.}
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
    \item{Content}{\code{signature(object = "NewsgroupDocument")}: Returns the
      text corpus, i.e., the actual character data slot.}
    \item{Content<-}{\code{signature(object = "NewsgroupDocument")}: Sets the
      text corpus, i.e., the actual character data slot.}
    \item{URI}{\code{signature(object = "NewsgroupDocument")}: Returns the
      filename on disk.}
    \item{Cached}{\code{signature(object = "NewsgroupDocument")}: Returns
      status information for loading on demand.}
    \item{Cached<-}{\code{signature(object = "NewsgroupDocument")}: Sets
      status information for loading on demand.}
  }
}
\references{\url{http://kdd.ics.uci.edu/databases/20newsgroups/20newsgroups.html}}
\author{Ingo Feinerer}
\keyword{classes}
