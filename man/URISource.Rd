\name{URISource}
\alias{URISource}
\title{Uniform Resource Identifier Source}
\description{  
  Constructs a source which represents a single document located by a
  uniform resource identifier.
}
\usage{
URISource(x, encoding = "UTF-8")
}
\arguments{
  \item{x}{The Uniform Resource Identifier, i.e., either a
    character identifying the file or a connection.}
  \item{encoding}{A character giving the encoding of \code{x}.}
}
\value{
  An object of class \code{URISource} which extends the class
  \code{Source} representing a single document located by a URI.
}
\author{Ingo Feinerer}
\seealso{
  \code{\link{DirSource}} for accessing multiple files, and
  \code{\link{getSources}} to list available sources.
}
\examples{
loreipsum <- system.file("texts", "loremipsum.txt", package = "tm")
us <- URISource(loreipsum)
inspect(Corpus(us))
}
