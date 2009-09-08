\name{removeNumbers}
\alias{removeNumbers}
\alias{removeNumbers.PlainTextDocument}
\title{Remove Numbers from a Text Document}
\description{
  Stip any numbers from a text document.
}
\usage{
\method{removeNumbers}{PlainTextDocument}(x)
}
\arguments{
  \item{x}{A text document.}
}
\value{
  The text document with any numbers in it removed.
}
\seealso{
  \code{\link{getTransformations}} to list available transformation (mapping) functions.
}
\examples{
data("crude")
crude[[1]]
removeNumbers(crude[[1]])
}
