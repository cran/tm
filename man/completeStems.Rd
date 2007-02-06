\name{stemCompletion}
\alias{stemCompletion}
\title{Complete Stems}
\description{
  Heuristically complete stemmed words.
}
\usage{
stemCompletion(object, words)
}
\arguments{
  \item{object}{a \code{Corpus} to be searched for possible completions.}
  \item{words}{a \code{character} vector of stems to be completed.}
}
\value{
  A character vector with completed \code{words}.
}
\examples{
data("crude")
stemCompletion(crude, c("compan", "entit", "suppl"))
}
\author{Ingo Feinerer}
\keyword{file}
