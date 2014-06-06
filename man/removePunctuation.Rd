\name{removePunctuation}
\alias{removePunctuation}
\alias{removePunctuation.character}
\alias{removePunctuation.PlainTextDocument}
\title{Remove Punctuation Marks from a Text Document}
\description{
  Remove punctuation marks from a text document.
}
\usage{
\method{removePunctuation}{character}(x, preserve_intra_word_dashes = FALSE)
\method{removePunctuation}{PlainTextDocument}(x, \dots)
}
\arguments{
  \item{x}{A character or text document.}
  \item{preserve_intra_word_dashes}{a logical specifying whether
    intra-word dashes should be kept.}
  \item{\dots}{passed over argument \code{preserve_intra_word_dashes}.}
}
\value{
  The character or text document \code{x} without punctuation marks
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
