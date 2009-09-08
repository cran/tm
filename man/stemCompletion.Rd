\name{stemCompletion}
\alias{stemCompletion}
\title{Complete Stems}
\description{
  Heuristically complete stemmed words.
}
\usage{
stemCompletion(x, words, type = c("prevalent", "first"))
}
\arguments{
  \item{x}{A \code{Corpus} to be searched for possible completions.}
  \item{words}{A \code{character} vector of stems to be completed.}
  \item{type}{A \code{character} naming the heuristics to be
    used: \code{prevalent} is default and takes the most frequent match as
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
