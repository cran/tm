\name{DataframeSource}
\alias{DataframeSource}
\title{Data Frame Source}
\description{
  Constructs a source from a data frame.
}
\usage{
DataframeSource(x, encoding = "unknown")
}
\arguments{
  \item{x}{A data frame holding the texts.}
  \item{encoding}{encoding to be assumed for input strings. It is used to mark
    character strings as known to be in Latin-1 or UTF-8: it is not used to
    re-encode the input.}
}
\value{
  An object of class \code{DataframeSource} which extends the class
  \code{Source} representing a data frame interpreting each row as a
  document.
}
\author{Ingo Feinerer}
\seealso{
  \code{\link{getSources}} to list available sources. \code{\link{Encoding}} on encodings in \R.
}
\examples{
docs <- data.frame(docs = c("This is a text.", "This another one."),
                   row.names = c("Text 1", "Text 2"))
(ds <- DataframeSource(docs))
inspect(Corpus(ds))
}
