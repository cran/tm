\name{CSVSource}
\docType{methods}
\alias{CSVSource}
\alias{CSVSource,character-method}
\alias{CSVSource,ANY-method}
\title{Comma Seperated Value Source}
\description{
  Constructs a source for a comma separated values file.
}
\usage{
\S4method{CSVSource}{character}(object)
}
\arguments{
  \item{object}{either a character identifying the file or a call which
    results in a connection.}
}
\value{
  An S4 object of class \code{CSVSource} which extends the class
  \code{Source} representing a comma separated values file.
}
\author{Ingo Feinerer}
\keyword{methods}
