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
\S4method{ReutersSource}{character}(object)
}
\arguments{
  \item{object}{either a character identifying the file or a call which
    results in a connection.}
}
\value{
  An S4 object of class \code{ReutersSource} which extends the class
  \code{Source} representing a Reuters XML document.
}
\author{Ingo Feinerer}
\keyword{methods}
