\name{DataframeSource-class}
\docType{class}
\alias{DataframeSource-class}
\title{Source for Data Frames}
\description{
  A class representing a data frame where each row is interpreted as a
  document.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("DataframeSource", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{DefaultReader}:}{Object of class \code{function}
      describing a default reader.}
    \item{\code{Encoding}:}{Object of class \code{character} holding the
      encoding of the texts delivered by the source.}
    \item{\code{Content}:}{Object of class \code{data.frame} holding the
      actual texts.}
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
\seealso{
  \code{\link{DataframeSource}}
}
\author{Ingo Feinerer}
\keyword{classes}
