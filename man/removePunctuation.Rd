\name{removePunctuation}
\alias{removePunctuation}
\alias{removePunctuation.character}
\alias{removePunctuation.PlainTextDocument}
\title{Remove Punctuation Marks from a Text Document}
\description{
  Remove punctuation marks from a text document.
}
\usage{
\method{removePunctuation}{character}(x,
                  preserve_intra_word_contractions = FALSE,
                  preserve_intra_word_dashes = FALSE,
                  ucp = FALSE, \dots)
\method{removePunctuation}{PlainTextDocument}(x, \dots)
}
\arguments{
  \item{x}{a character vector or text document.}
  \item{preserve_intra_word_contractions}{a logical specifying whether
    intra-word contractions should be kept.}
  \item{preserve_intra_word_dashes}{a logical specifying whether
    intra-word dashes should be kept.}
  \item{ucp}{a logical specifying whether to use Unicode character
    properties for determining punctuation characters.  If \code{FALSE} 
    (default), characters in the ASCII \code{[:punct:]} class are taken;
    if \code{TRUE}, the characters with Unicode general category
    \code{P} (Punctuation).}
  \item{\dots}{arguments to be passed to or from methods;
    in particular, from the \code{PlainTextDocument} method to the
    \code{character} method.}
}
\value{
  The character or text document \code{x} without punctuation marks
  (besides intra-word contractions (\samp{'}) and intra-word dashes
  (\samp{-}) if \code{preserve_intra_word_contractions} and
  \code{preserve_intra_word_dashes} are set, respectively).
}
\seealso{
  \code{\link{getTransformations}} to list available transformation
  (mapping) functions.

  \code{\link{regex}} shows the class \code{[:punct:]} of punctuation
  characters.

  \url{https://unicode.org/reports/tr44/#General_Category_Values}.
}
\examples{
data("crude")
inspect(crude[[14]])
inspect(removePunctuation(crude[[14]]))
inspect(removePunctuation(crude[[14]],
                          preserve_intra_word_contractions = TRUE,
                          preserve_intra_word_dashes = TRUE))
}
