\name{DirSource}
\docType{methods}
\alias{DirSource}
\alias{DirSource,character-method}
\title{Directory Source}
\description{
  Constructs a directory source.
}
\usage{
\S4method{DirSource}{character}(directory, encoding = "UTF-8", pattern = NULL, recursive = FALSE, ignore.case = FALSE)
}
\arguments{
  \item{directory}{a directory.}
  \item{encoding}{a character giving the encoding of the files in the directory.}
  \item{pattern}{an optional regular expression. Only file names which match the regular expression will be returned.}
  \item{recursive}{logical. Should the listing recurse into directories?}
  \item{ignore.case}{logical. Should pattern-matching be case-insensitive?}
}
\value{
  An S4 object of class \code{DirSource} which extends the class
  \code{Source} representing a directory.
}
\author{Ingo Feinerer}
\keyword{methods}
