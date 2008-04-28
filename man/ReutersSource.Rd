\name{ReutersSource}
\docType{methods}
\alias{ReutersSource}
\alias{ReutersSource,character-method}
\alias{ReutersSource,ANY-method}
\title{Reuters Source}
\description{
  Constructs a source for a Reuters XML document.
}
\usage{
\S4method{ReutersSource}{character}(object, encoding = "UTF-8")
}
\arguments{
  \item{object}{Either a character identifying the file or a connection.}
  \item{encoding}{A character giving the encoding of the file.}
}
\value{
  An S4 object of class \code{ReutersSource} which extends the class
  \code{Source} representing a Reuters XML document.
}
\author{Ingo Feinerer}
\keyword{methods}
