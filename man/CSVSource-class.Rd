\name{CSVSource-class}
\docType{class}
\alias{CSVSource-class}
\title{Source for Comma Separated Files}
\description{
  A class representing a comma separated file, where each line stands
  for a text document.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("CSVSource", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{DefaultReader}:}{Object of class \code{function}
      describing a default reader}
    \item{\code{URI}:}{Object of class \code{character} describing the
      connection call to be made for accessing the document physically}
    \item{\code{Content}:}{Object of class \code{character} holding the
      CSV file corpus}
    \item{\code{LoDSupport}:}{Object of class \code{logical}
      indicating Load on Demand Support}
    \item{\code{Position}:}{Object of class \code{numeric} giving the
      position in the source}
  }
}
\section{Extends}{
  Class \code{Source}, directly.
}
\author{Ingo Feinerer}
\keyword{classes}
