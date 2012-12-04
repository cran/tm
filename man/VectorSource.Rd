\name{VectorSource}
\alias{VectorSource}
\title{Vector Source}
\description{
  Constructs a source for a vector as input.
}
\usage{
VectorSource(x, encoding = "unknown")
}
\arguments{
  \item{x}{A vector.}
  \item{encoding}{encoding to be assumed for input strings.  It is used to mark
    character strings as known to be in Latin-1 or UTF-8: it is not used to
    re-encode the input.}
}
\value{
  An object of class \code{VectorSource} which extends the class
  \code{Source} representing a vector where each entry is interpreted as
  a document.
}
\seealso{
  \code{\link{getSources}} to list available sources. \code{\link{Encoding}} on encodings in \R.
}
\examples{
docs <- c("This is a text.", "This another one.")
(vs <- VectorSource(docs))
inspect(Corpus(vs))
}
\author{Ingo Feinerer}
