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
    \item{\code{DefaultReader}:}{object of class \code{function}
      describing a default reader.}
    \item{\code{Encoding}:}{object of class \code{character} holding the
      encoding of the texts delivered by the source.}
    \item{\code{Content}:}{object of class \code{vector} holding the
      actual texts.}
    \item{\code{LoDSupport}:}{object of class \code{logical}
      indicating Load on Demand Support.}
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
\seealso{
  \code{\link{VectorSource}}
}
\author{Ingo Feinerer}
\keyword{classes}
