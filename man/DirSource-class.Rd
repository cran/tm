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
    \item{\code{DefaultReader}:}{Object of class \code{function}
      describing a default reader.}
    \item{\code{Encoding}:}{Object of class \code{character} holding the
      encoding of the texts delivered by the source.}
    \item{\code{FileList}:}{Object of class \code{character} giving the
      file list in the directory.}
    \item{\code{LoDSupport}:}{Object of class \code{logical}
      indicating Load on Demand Support.}
    \item{\code{Position}:}{Object of class \code{numeric} giving the
      position in the source.}
    \item{\code{Length}:}{Object of class \code{numeric} denoting the
      number of the elements delivered by the source.}
  }
}
\author{Ingo Feinerer}
\section{Extends}{
  Class \code{Source}, directly.
}
\keyword{classes}
