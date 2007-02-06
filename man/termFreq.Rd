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
  \item{doc}{an object inheriting from \code{TextDocument}.}
  \item{control}{a list of control options. Possible settings are
    \itemize{
      \item \code{tolower}: a function converting characters to lower
      case. Defaults to \code{base::tolower}.
      \item \code{tokenize}: a function tokenizing documents to single
      tokens. Defaults to \code{function(x) unlist(strsplit(gsub("[^[:alnum:]]+", " ", x), " ", fixed = TRUE)}.
      \item \code{removeNumbers}: a Boolean value indicating whether
      numbers should be removed from \code{doc}.
      \item \code{stemming}: a Boolean value indicating whether tokens
      should be stemmed. Defaults to \code{FALSE}.
      \item \code{stopwords}: either a Boolean value indicating stopword
      removal using default language specific stopword lists shipped
      with this package or a character vector holding custom stopwords.
      \item \code{dictionary}: a character vector to be tabulated
      against. No other terms will be listed in the result. Defaults to
      no action (i.e., all terms are considered).
      \item \code{minDocFreq}: an integer value. Words that appear less
      often in \code{doc} than this number are discarded. Defaults to
      \code{1} (i.e., every token will be used).
      \item \code{minWordLength}: an integer value. Words smaller than
      this number are discarded. Defaults to length \code{3}.
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
termFreq(crude[[1]], control = list(stemming = TRUE, minWordLength = 4))
}
\keyword{math}
