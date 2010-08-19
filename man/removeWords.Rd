\name{removeWords}
\alias{removeWords}
\alias{removeWords.PlainTextDocument}
\title{Remove Words from a Text Document}
\description{
  Remove a set of words from a text document.
}
\usage{
\method{removeWords}{PlainTextDocument}(x, words)
}
\arguments{
  \item{x}{A text document.}
  \item{words}{A character vector listing the words to be removed.}
}
\value{
  The text document with the specified words in it removed.
}
\seealso{
  \code{\link{getTransformations}} to list available transformation (mapping) functions.
}
\examples{
data("crude")
crude[[1]]
removeWords(crude[[1]], stopwords("english"))
}
