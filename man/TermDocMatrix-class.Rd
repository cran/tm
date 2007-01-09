\name{TermDocMatrix-class}
\docType{class}
\alias{TermDocMatrix-class}
\alias{Weighting}
\alias{Weighting<-}
\alias{Weighting,TermDocMatrix-method}
\alias{Weighting<-,TermDocMatrix-method}
\title{Term-document matrix}
\description{
  A class representing a term-document matrix. A column represents a
  term, and a row the ID of a text document, respectively.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{new("TermDocMatrix",...)} or by calling the function
  \code{TermDocMatrix}.
}
\section{Slots}{
  \describe{
    \item{\code{Weighting}:}{Object of class \code{"character"} containing
      the mode which was applied on the matrix. Possible are term
      frequency \code{"tf"}, term frequency-inverse document frequency
      \code{"tf-idf"}, binary \code{"bin"} and logical \code{"logical"}.}
  }
}
\section{Extends}{
Class \code{"matrix"}, directly.
}
\section{Methods}{
  \describe{
    \item{Weighting}{\code{signature(object = "TermDocMatrix")}: returns the
      weighting mode}
    \item{Weighting<-}{\code{signature(object = "TermDocMatrix")}: sets the
      weighting mode}
  }
}
\author{Ingo Feinerer}
\seealso{
  \code{\link{TermDocMatrix}}
}
\keyword{classes}
