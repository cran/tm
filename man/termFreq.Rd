\name{termFreq}
\alias{termFreq}
\title{Term Frequency Vector}
\description{
  Generate a term frequency vector from a text document.
}
\usage{
termFreq(doc, control = list())
}
\arguments{
  \item{doc}{An object inheriting from \code{TextDocument}.}
  \item{control}{A list of control options. Possible settings are
    \describe{
      \item{\code{tolower}}{A function converting characters to lower
	case. Defaults to \code{\link{tolower}}.}
      \item{\code{removePunctuation}}{A logical value indicating whether
	punctuation characters should be removed from
	\code{doc}. Defaults to \code{FALSE}.}
      \item{\code{tokenize}}{A function tokenizing documents into single
	tokens. Defaults to \code{scan(\dots, what = "character")}.}
      \item{\code{removeNumbers}}{A logical value indicating whether
	numbers should be removed from \code{doc}. Defaults to \code{FALSE}.}
      \item{\code{stemming}}{Either a Boolean value indicating whether tokens
	should be stemmed or a stemming function. Defaults to \code{FALSE}.}
      \item{\code{stopwords}}{Either a Boolean value indicating stopword
	removal using default language specific stopword lists shipped
	with this package or a character vector holding custom
	stopwords. Defaults to \code{FALSE}.}
      \item{\code{dictionary}}{A character vector to be tabulated
	against. No other terms will be listed in the result. Defaults
	to \code{NULL} which means that all terms in \code{doc} are
	listed.}
      \item{\code{minDocFreq}}{An integer value. Words that appear less
	often in \code{doc} than this number are discarded. Defaults to
	\code{1} (i.e., every token will be used).}
      \item{\code{minWordLength}}{An integer value. Words smaller than
	this number are discarded. Defaults to length \code{3}.}
    }
  }
}
\value{
  A named integer vector with term frequencies as values and tokens as
  names.
}
\examples{
data("crude")
termFreq(crude[[1]])
termFreq(crude[[1]],
         control = list(removePunctuation = TRUE, stemming = TRUE,
                        minWordLength = 4))
}
\keyword{math}
