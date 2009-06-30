\name{TextRepository-class}
\docType{class}
\alias{TextRepository-class}
\alias{RepoMetaData}
\alias{RepoMetaData,TextRepository-method}
\title{Text Repository}
\description{
  A class representing a repository of corpora.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{new("TextRepository",...)} or by calling the function
  \code{TextRepository}.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{list} containing the
      corpora.}
    \item{\code{RepoMetaData}:}{Object of class \code{list}
      containing meta data for the text document
      collections in form of tag-value pairs.}
  }
}
\section{Extends}{
  Class \code{list}, directly.
}
\section{Methods}{
  \describe{
    \item{RepoMetaData}{\code{signature(object = "TextRepository")}:
      returns meta data in form of a tag-value paired
      list.}
  }
}
\author{Ingo Feinerer}
\seealso{
  \code{\link{TextRepository}}
}
\keyword{classes}
