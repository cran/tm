\name{removePunctuation}
\alias{removePunctuation}
\alias{removePunctuation.PlainTextDocument}
\title{Remove Punctuation Marks from a Text Document}
\description{
  Remove all punctuation marks from a text document.
}
\usage{
\method{removePunctuation}{PlainTextDocument}(x)
}
\arguments{
  \item{x}{A text document.}
}
\value{
  The text document with any punctuation marks in it removed.
}
\seealso{
  \code{\link{getTransformations}} to list available transformation (mapping) functions.
}
\examples{
data("crude")
crude[[1]]
removePunctuation(crude[[1]])
}
\keyword{methods}
