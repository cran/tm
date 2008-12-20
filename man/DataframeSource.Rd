\name{DataframeSource}
\alias{DataframeSource}
\title{Data Frame Source}
\description{
  Constructs a source from a data frame.
}
\usage{
DataframeSource(object, encoding = "UTF-8")
}
\arguments{
  \item{object}{A data frame holding the texts.}
  \item{encoding}{A character giving the encoding of the file.}
}
\value{
  An S4 object of class \code{DataframeSource} which extends the class
  \code{Source} representing a data frame interpreting each row as a
  document.
}
\author{Ingo Feinerer}
\seealso{\code{\link{DataframeSource-class}}}
\examples{
docs <- data.frame(docs = c("This is a text.", "This another one."))
(ds <- DataframeSource(docs))
inspect(Corpus(ds))
}
\keyword{methods}
