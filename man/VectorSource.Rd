\name{VectorSource}
\alias{VectorSource}
\title{Vector Source}
\description{
  Constructs a source for a vector as input.
}
\usage{
VectorSource(x, encoding = "UTF-8")
}
\arguments{
  \item{x}{A vector.}
  \item{encoding}{A character giving the encoding of \code{x}.}
}
\value{
  An object of class \code{VectorSource} which extends the class
  \code{Source} representing a vector where each entry is interpreted as
  a document.
}
\seealso{
  \code{\link{getSources}} to list available sources.
}
\examples{
docs <- c("This is a text.", "This another one.")
(vs <- VectorSource(docs))
inspect(Corpus(vs))
}
\author{Ingo Feinerer}
