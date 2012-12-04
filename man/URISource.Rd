\name{URISource}
\alias{URISource}
\title{Uniform Resource Identifier Source}
\description{  
  Constructs a source which represents a single document located by a
  uniform resource identifier.
}
\usage{
URISource(x, encoding = "unknown")
}
\arguments{
  \item{x}{The Uniform Resource Identifier, i.e., either a
    character identifying the file or a connection.}
  \item{encoding}{encoding to be assumed for input strings.  It is used to mark
    character strings as known to be in Latin-1 or UTF-8: it is not used to
    re-encode the input.}
}
\value{
  An object of class \code{URISource} which extends the class
  \code{Source} representing a single document located by a URI.
}
\author{Ingo Feinerer}
\seealso{
  \code{\link{DirSource}} for accessing multiple files, and
  \code{\link{getSources}} to list available sources.
  \code{\link{Encoding}} on encodings in \R.
}
\examples{
loremipsum <- system.file("texts", "loremipsum.txt", package = "tm")
us <- URISource(loremipsum)
inspect(Corpus(us))
}
