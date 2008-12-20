\name{URISource}
\docType{methods}
\alias{URISource}
\alias{URISource,character-method}
\alias{URISource,connection-method}
\title{Uniform Resource Identifier Source}
\description{
  Constructs a Uniform Resource Identifier source.
}
\usage{
\S4method{URISource}{character}(object, encoding = "UTF-8")
}
\arguments{
  \item{object}{the Uniform Resource Identifier, i.e., either a
    character identifying the file or a connection.}
  \item{encoding}{a character giving the encoding of the file.}
}
\value{
  An S4 object of class \code{URISource} which extends the class
  \code{Source} representing a single document identified by a URI.
}
\author{Ingo Feinerer}
\seealso{\code{\link{DirSource}} for accessing multiple files.}
\keyword{methods}
