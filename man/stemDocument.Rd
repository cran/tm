\name{stemDocument}
\alias{stemDocument}
\alias{stemDocument.character}
\alias{stemDocument.PlainTextDocument}
\title{Stem Words}
\description{
  Stem words in a text document using Porter's stemming algorithm.
}
\usage{
\method{stemDocument}{character}(x, language = "english")
\method{stemDocument}{PlainTextDocument}(x, language = meta(x, "language"))
}
\arguments{
  \item{x}{A character vector or text document.}
  \item{language}{A string giving the language for stemming.}
}
\details{
  The argument \code{language} is passed over to
  \code{\link[SnowballC]{wordStem}} as the name of the Snowball stemmer.
}
\examples{
data("crude")
inspect(crude[[1]])
inspect(stemDocument(crude[[1]]))
}
