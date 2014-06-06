\name{removeWords}
\alias{removeWords}
\alias{removeWords.character}
\alias{removeWords.PlainTextDocument}
\title{Remove Words from a Text Document}
\description{
  Remove words from a text document.
}
\usage{
\method{removeWords}{character}(x, words)
\method{removeWords}{PlainTextDocument}(x, \dots)
}
\arguments{
  \item{x}{A character or text document.}
  \item{words}{A character vector giving the words to be removed.}
  \item{\dots}{passed over argument \code{words}.}
}
\value{
  The character or text document without the specified words.
}
\seealso{
  \code{\link{getTransformations}} to list available transformation (mapping)
  functions.

  \code{\link[tau]{remove_stopwords}} provided by package \pkg{tau}.
}
\examples{
data("crude")
crude[[1]]
removeWords(crude[[1]], stopwords("english"))
}
