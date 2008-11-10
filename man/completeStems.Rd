\name{stemCompletion}
\alias{stemCompletion}
\title{Complete Stems}
\description{
  Heuristically complete stemmed words.
}
\usage{
stemCompletion(object, words, type = c("prevalent", "first"))
}
\arguments{
  \item{object}{a \code{Corpus} to be searched for possible completions.}
  \item{words}{a \code{character} vector of stems to be completed.}
  \item{type}{a \code{character} naming the heuristics to be
    used. \code{prevalent} is default and takes the most frequent match as
    completion, whereas \code{first} takes the first found completion.}
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
