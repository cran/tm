\name{XMLSource-class}
\docType{class}
\alias{XMLSource-class}
\title{Source for XML Files}
\description{
  A class representing an XML source containing multiple documents.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("XMLSource", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{DefaultReader}:}{object of class \code{function}
      describing a default reader.}
    \item{\code{Encoding}:}{object of class \code{character} holding the
      encoding of the texts delivered by the source.}
    \item{\code{URI}:}{object of class \code{character} describing the
      connection call to be made for accessing the document physically.}
    \item{\code{Content}:}{object of class \code{list} holding the
      XML source content.}
    \item{\code{LoDSupport}:}{object of class \code{logical} indicating
      Load on Demand Support.}
    \item{\code{Position}:}{object of class \code{numeric} giving the
      position in the source.}
    \item{\code{Length}:}{object of class \code{numeric} denoting the
      number of the elements delivered by the source.}
    \item{\code{Vectorized}:}{object of class \code{logical} indicating
      the ability for parallel element access.}
  }
}
\section{Extends}{
  Class \code{\linkS4class{Source}}, directly.
}
\author{Ingo Feinerer}
\keyword{classes}
