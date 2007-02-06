\name{NewsgroupDocument-class}
\docType{class}
\alias{NewsgroupDocument-class}
\alias{Content,NewsgroupDocument-method}
\alias{Content<-,NewsgroupDocument-method}
\alias{URI,NewsgroupDocument-method}
\alias{Cached,NewsgroupDocument-method}
\alias{Cached<-,NewsgroupDocument-method}
\title{Text document}
\description{
  A class representing a newsgroup document with additional
  information. The newsgroup documents must be formatted according to
  the Newsgroup dataset from the UCI KDD archive.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("NewsgroupDocument", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{Newsgroup}:}{Object of class \code{character} containing
      the newsgroups where the document has been posted}
    \item{\code{URI}:}{Object of class \code{character} containing
      the path and filename holding the data physically on disk}
    \item{\code{Cached}:}{Object of class \code{numeric} containing the
      status whether the file was already loaded in memory. Zero indicates
      not loaded, a value greater zero indicates in memory.}
  }
}
\section{Extends}{
Class \code{character} and \code{TextDocument}, directly.
}
\section{Methods}{
  \describe{
    \item{Content}{\code{signature(object = "NewsgroupDocument")}: returns the
      text corpus, i.e., the actual character data slot}
    \item{Content<-}{\code{signature(object = "NewsgroupDocument")}: sets the
      text corpus, i.e., the actual character data slot}
    \item{URI}{\code{signature(object = "NewsgroupDocument")}: returns the
      filename on disk}
    \item{Cached}{\code{signature(object = "NewsgroupDocument")}: returns
      status information for loading on demand}
    \item{Cached<-}{\code{signature(object = "NewsgroupDocument")}: sets
      status information for loading on demand}
  }
}
\references{\url{http://kdd.ics.uci.edu/databases/20newsgroups/20newsgroups.html}}
\author{Ingo Feinerer}
\keyword{classes}
