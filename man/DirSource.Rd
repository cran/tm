\name{DirSource}
\alias{DirSource}
\title{Directory Source}
\description{
  Constructs a directory source.
}
\usage{
DirSource(directory = ".", encoding = "UTF-8", pattern = NULL,
          recursive = FALSE, ignore.case = FALSE)
}
\arguments{
  \item{directory}{A character vector of full path names; the default corresponds to the working directory \code{getwd()}.}
  \item{encoding}{A character giving the encoding of the files in \code{directory}.}
  \item{pattern}{An optional regular expression. Only file names which match the regular expression will be returned.}
  \item{recursive}{Logical. Should the listing recurse into directories?}
  \item{ignore.case}{Logical. Should pattern-matching be case-insensitive?}
}
\value{
  An object of class \code{DirSource} which extends the class
  \code{Source} representing a directory. Each file in this directory is
  considered to be a document.
}
\author{Ingo Feinerer}
\seealso{
  \code{\link{getSources}} to list available sources.
}
\examples{
DirSource(system.file("texts", "txt", package = "tm"))
}
