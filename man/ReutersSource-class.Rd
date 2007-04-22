\name{ReutersSource-class}
\docType{class}
\alias{ReutersSource-class}
\title{Source for Reuters Files}
\description{
  A class representing either a Reuters21578 or RCV1 source, where
  several documents are found in a single source (i.e., typically file).
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("ReutersSource", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{DefaultReader}:}{Object of class \code{function}
      describing a default reader}
    \item{\code{URI}:}{Object of class \code{character} describing the
      connection call to be made for accessing the document physically}
    \item{\code{Content}:}{Object of class \code{list} holding the
      Reuters XML source corpus}
    \item{\code{LoDSupport}:}{Object of class \code{logical} indicating
      Load on Demand Support}
    \item{\code{Position}:}{Object of class \code{numeric} giving the
      position in the source}
  }
}
\section{Extends}{
  Class \code{Source}, directly.
}
\author{Ingo Feinerer}
\keyword{classes}
