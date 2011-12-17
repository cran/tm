\name{removePunctuation}
\alias{removePunctuation}
\alias{removePunctuation.PlainTextDocument}
\title{Remove Punctuation Marks from a Text Document}
\description{
  Remove punctuation marks from a text document.
}
\usage{
\method{removePunctuation}{PlainTextDocument}(x, preserve_intra_word_dashes = FALSE)
}
\arguments{
  \item{x}{A text document.}
  \item{preserve_intra_word_dashes}{A logical specifying whether
    intra-word dashes should be kept.}
}
\value{
  The text document \code{x} with any punctuation marks in it removed
  (besides intra-word dashes if \code{preserve_intra_word_dashes} is
  set).
}
\seealso{
  \code{\link{getTransformations}} to list available transformation
  (mapping) functions.

  \code{\link{regex}} shows the class \code{[:punct:]} of punctuation
  characters.
}
\examples{
data("crude")
crude[[14]]
removePunctuation(crude[[14]])
removePunctuation(crude[[14]], preserve_intra_word_dashes = TRUE)
}
\keyword{methods}
