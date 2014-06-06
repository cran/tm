\name{stemDocument}
\alias{stemDocument}
\alias{stemDocument.PlainTextDocument}
\title{Stem Words}
\description{
  Stem words in a text document using Porter's stemming algorithm.
}
\usage{
\method{stemDocument}{PlainTextDocument}(x, language = meta(x, "language"))
}
\arguments{
  \item{x}{A text document.}
  \item{language}{A character giving the language for stemming.}
}
\details{
  The argument \code{language} is passed over to
  \code{\link[SnowballC]{wordStem}} as the name of the Snowball stemmer.
}
\examples{
data("crude")
crude[[1]]
stemDocument(crude[[1]])
}
