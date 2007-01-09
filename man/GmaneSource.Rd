\name{GmaneSource}
\docType{methods}
\alias{GmaneSource}
\alias{GmaneSource,character-method}
\alias{GmaneSource,ANY-method}
\title{Gmane Source}
\description{
  Constructs a source for a Gmane mailing list RSS feed.
}
\usage{
\S4method{GmaneSource}{character}(object)
}
\arguments{
  \item{object}{either a character identifying the file or a call which
    results in a connection.}
}
\value{
  An S4 object of class \code{GmaneSource} which extends the class
  \code{Source} representing a Gmane mailing list RSS feed.
}
\author{Ingo Feinerer}
\keyword{methods}
