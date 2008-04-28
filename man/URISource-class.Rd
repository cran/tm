\name{URISource-class}
\docType{class}
\alias{URISource-class}
\title{Source for Directories}
\description{
  A class representing a single document identified by a Uniform
  Resource Identifier.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("URISource", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{DefaultReader}:}{object of class \code{function}
      describing a default reader.}
    \item{\code{Encoding}:}{object of class \code{character} holding the
      encoding of the text delivered by the source.}
    \item{\code{URI}:}{object of class \code{call} describing the
      connection call to be made for accessing the document physically.}
    \item{\code{LoDSupport}:}{object of class \code{logical}
      indicating Load on Demand Support.}
    \item{\code{Position}:}{object of class \code{numeric} giving the
      position in the source.}
    \item{\code{Length}:}{object of class \code{numeric} denoting the
      number of the elements delivered by the source, i.e., one as only
      a single document is returned by this source.}
    \item{\code{Vectorized}:}{object of class \code{logical} indicating
      the ability for parallel element access.}
  }
}
\author{Ingo Feinerer}
\section{Extends}{
  Class \code{\linkS4class{Source}}, directly.
}
\keyword{classes}
