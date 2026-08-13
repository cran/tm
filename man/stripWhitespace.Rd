\name{stripWhitespace}
\alias{stripWhitespace}
\alias{stripWhitespace.PlainTextDocument}
\alias{stripWhitespace.character}
\title{Strip Whitespace from a Text Document}
\description{
  Strip extra whitespace from a text document. Multiple whitespace
  characters are collapsed to a single blank.
}
\usage{
stripWhitespace(x, \dots)
\method{stripWhitespace}{PlainTextDocument}(x, \dots)
\method{stripWhitespace}{character}(x, \dots)
}
\arguments{
  \item{x}{A text document.}
  \item{\dots}{arguments to be passed to or from methods.
    Currently not used.}
}
\value{
  The text document with multiple whitespace characters collapsed to a
  single blank.
}
\seealso{
  \code{\link{getTransformations}} to list available transformation (mapping)
  functions.
}
\examples{
data("crude")
crude[[1]]
stripWhitespace(crude[[1]])
}
