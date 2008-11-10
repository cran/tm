\name{VectorSource}
\docType{methods}
\alias{VectorSource}
\alias{VectorSource,vector-method}
\alias{VectorSource,ANY-method}
\title{Gmane Source}
\description{
  Constructs a source for a vector.
}
\usage{
\S4method{VectorSource}{vector}(object, encoding = "UTF-8")
}
\arguments{
  \item{object}{A vector holding the texts.}
  \item{encoding}{A character giving the encoding of the file.}
}
\value{
  An S4 object of class \code{VectorSource} which extends the class
  \code{Source} representing a vector interpreting each element as a
  document.
}
\examples{
docs <- c("This is a text.", "This another one.")
(vs <- VectorSource(docs))
inspect(Corpus(vs))
}
\author{Ingo Feinerer}
\keyword{methods}
