\name{DirSource}
\docType{methods}
\alias{DirSource}
\alias{DirSource,character-method}
\title{Directory Source}
\description{
  Constructs a directory source.
}
\usage{
\S4method{DirSource}{character}(directory, encoding = "UTF-8", recursive = FALSE)
}
\arguments{
  \item{directory}{a directory.}
    \item{encoding}{a character giving the encoding of the files in the
      directory.}
  \item{recursive}{a logical value indicating whether the directory
    should be traversed recursively.}
}
\value{
  An S4 object of class \code{DirSource} which extends the class
  \code{Source} representing a directory.
}
\author{Ingo Feinerer}
\keyword{methods}
