\name{stemDocument}
\alias{stemDocument}
\alias{stemDocument.PlainTextDocument}
\title{Stem Words}
\description{
  Stem words in a text document using Porter's stemming algorithm.
}
\usage{
\method{stemDocument}{PlainTextDocument}(x, language = "english")
}
\arguments{
  \item{x}{A text document.}
  \item{language}{A character setting the language to be used for stemming.}
}
\details{
  The argument \code{language} is passed over to
  \code{\link[Snowball:SnowballStemmer]{SnowballStemmer}} as the name of
  the snowball stemmer.
}
\examples{
data("crude")
crude[[1]]
stemDocument(crude[[1]])
}
