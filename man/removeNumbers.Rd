\name{removeNumbers}
\alias{removeNumbers}
\alias{removeNumbers.PlainTextDocument}
\title{Remove Numbers from a Text Document}
\description{
  Remove numbers from a text document.
}
\usage{
\method{removeNumbers}{PlainTextDocument}(x, \dots)
}
\arguments{
  \item{x}{A text document.}
  \item{\dots}{Not used.}
}
\value{
  The text document without numbers.
}
\seealso{
  \code{\link{getTransformations}} to list available transformation (mapping)
  functions.
}
\examples{
data("crude")
crude[[1]]
removeNumbers(crude[[1]])
}
