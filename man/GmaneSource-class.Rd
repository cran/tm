\name{GmaneSource-class}
\docType{class}
\alias{GmaneSource-class}
\title{Source for Gmane Feeds}
\description{
  A class representing a Gmane RSS feed.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("GmaneSource", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{DefaultReader}:}{Object of class \code{function}
      describing a default reader.}
    \item{\code{Encoding}:}{Object of class \code{character} holding the
      encoding of the texts delivered by the source.}
    \item{\code{URI}:}{Object of class \code{character} describing the
      connection call to be made for accessing the document physically.}
    \item{\code{Content}:}{Object of class \code{character} holding the
      RSS feed.}
    \item{\code{LoDSupport}:}{Object of class \code{logical}
      indicating Load on Demand Support.}
    \item{\code{Position}:}{Object of class \code{numeric} giving the
      position in the source.}
    \item{\code{Length}:}{Object of class \code{numeric} denoting the
      number of the elements delivered by the source.}
  }
}
\section{Extends}{
  Class \code{Source}, directly.
}
\author{Ingo Feinerer}
\keyword{classes}
