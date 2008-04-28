\name{DirSource-class}
\docType{class}
\alias{DirSource-class}
\title{Source for Directories}
\description{
  A class representing a directory. Each file in this directory is
  considered to be a document.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("DirSource", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{DefaultReader}:}{object of class \code{function}
      describing a default reader.}
    \item{\code{Encoding}:}{object of class \code{character} holding the
      encoding of the texts delivered by the source.}
    \item{\code{FileList}:}{object of class \code{character} giving the
      file list in the directory.}
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
\author{Ingo Feinerer}
\section{Extends}{
  Class \code{\linkS4class{Source}}, directly.
}
\keyword{classes}
