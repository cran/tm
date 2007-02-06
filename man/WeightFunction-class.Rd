\name{WeightFunction-class}
\docType{class}
\alias{WeightFunction-class}
\title{Weighting Function}
\description{
  A class representing a weighting function for a term-document matrix.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("WeightFunction", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}}{Object of class \code{function} holding the
      weighting function.}
    \item{\code{Name}}{Object of class \code{character} containing a
      name representation for the weighting function.}
  }
}
\section{Extends}{
  Class \code{function}, directly.
}
\author{Ingo Feinerer}
\keyword{classes}
