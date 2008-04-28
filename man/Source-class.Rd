\name{Source-class}
\docType{class}
\alias{Source-class}
\title{Source}
\description{
  A (virtual) class representing a source, e.g., a directory, a
  connection, et cetera.
}
\section{Objects from the Class}{
Objects can be created by deriving and instantiating subclasses.
}
\section{Slots}{
  \describe{
    \item{\code{DefaultReader}:}{object of class \code{function}
      describing a default reader.}
    \item{\code{Encoding}:}{object of class \code{character} holding the
      encoding of the texts delivered by the source.}
    \item{\code{Length}:}{object of class \code{numeric} denoting the
      number of the elements delivered by the source. If the number
      cannot be determined in advance it should be set to zero.}
    \item{\code{LoDSupport}:}{object of class \code{logical} indicating
      whether this source supports load on demand.}
    \item{\code{Position}:}{object of class \code{numeric} indicating
      the position in the source.}
    \item{\code{Vectorized}:}{object of class \code{logical} indicating
      the ability for parallel element access.}
  }
}
\seealso{
  \code{\link{stepNext}}, \code{\link{getElem}}, \code{\link{eoi}} for
  sequential element access, and \code{\link{pGetElem}} for parallel
  element access.

  \code{\link{getSources}} to list available sources.
}
\author{Ingo Feinerer}
\keyword{classes}
