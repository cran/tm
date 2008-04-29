\name{VectorSource-class}
\docType{class}
\alias{VectorSource-class}
\title{Source for Vectors}
\description{
  A class representing a vector where each component is interpreted as a
  document.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("VectorSource", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{DefaultReader}:}{Object of class \code{function}
      describing a default reader.}
    \item{\code{Encoding}:}{Object of class \code{character} holding the
      encoding of the texts delivered by the source.}
    \item{\code{Content}:}{Object of class \code{vector} holding the
      actual texts.}
    \item{\code{LoDSupport}:}{Object of class \code{logical}
      indicating Load on Demand Support.}
    \item{\code{Position}:}{Object of class \code{numeric} giving the
      position in the source.}
  }
}
\section{Extends}{
  Class \code{Source}, directly.
}
\author{Ingo Feinerer}
\keyword{classes}
