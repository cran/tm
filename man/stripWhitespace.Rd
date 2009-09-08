\name{stripWhitespace}
\alias{stripWhitespace}
\alias{stripWhitespace.PlainTextDocument}
\title{Strip Whitespace from a Text Document}
\description{
  Strip extra whitespace from a text document. Multiple white space
  characters are collapsed to a single blank.
}
\usage{
\method{stripWhitespace}{PlainTextDocument}(x)
}
\arguments{
  \item{x}{A text document.}
}
\value{
  The text document with multiple white space characters collapse to a
  single blank.
}
\seealso{
  \code{\link{getTransformations}} to list available transformation (mapping) functions.
}
\examples{
data("crude")
crude[[1]]
stripWhitespace(crude[[1]])
}
