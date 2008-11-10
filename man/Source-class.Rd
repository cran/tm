\name{Source-class}
\docType{class}
\alias{Source-class}
\title{Source Manager}
\description{
  A class representing a source, e.g., a directory, a connection, et cetera.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Source", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{DefaultReader}:}{Object of class \code{function}
      describing a default reader.}
    \item{\code{LoDSupport}:}{Object of class \code{logical} indicating
      where this source supports load on demand.}
    \item{\code{Position}:}{Object of class \code{numeric} indicating
      the position in the source.}
    \item{\code{Encoding}:}{Object of class \code{character} holding the
      encoding of the texts delivered by the source.}
    \item{\code{Length}:}{Object of class \code{numeric} denoting the
      number of the elements delivered by the source. If the number
      cannot be determined in advance it should be set to zero.}
  }
}
\author{Ingo Feinerer}
\keyword{classes}
