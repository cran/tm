\name{stopwords}
\alias{stopwords}
\title{Multilingual Stopwords}
\description{
  Return stopwords in different languages.
}
\usage{
stopwords(language = "eng")
}
\arguments{
  \item{language}{A \code{character} giving the desired language.}
}
\details{
  At the moment supported languages are \code{danish},
  \code{dutch}, \code{english}, \code{finnish}, \code{french},
  \code{german}, \code{hungarian}, \code{italian}, \code{norwegian},
  \code{portuguese}, \code{russian}, \code{spanish}, and
  \code{swedish}. Alternatively, their \acronym{ISO} 639-2 code may be
  used.
}
\value{A character vector containing the requested stopwords.}
\examples{
stopwords("eng")
}
\keyword{file}
