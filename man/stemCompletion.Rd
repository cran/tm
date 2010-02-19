\name{stemCompletion}
\alias{stemCompletion}
\title{Complete Stems}
\description{
  Heuristically complete stemmed words.
}
\usage{
stemCompletion(x, words, type = c("prevalent", "first", "longest", "none", "random", "shortest"))
}
\arguments{
  \item{x}{A \code{Corpus} to be searched for possible completions.}
  \item{words}{A \code{character} vector of stems to be completed.}
  \item{type}{A \code{character} naming the heuristics to be used:
    \describe{
      \item{\code{prevalent}}{Default. Takes the most frequent match as
      completion.}
      \item{\code{first}}{Takes the first found completion.}
      \item{\code{longest}}{Takes the longest completion in terms of
	characters.}
      \item{\code{none}}{Is the identity.}
      \item{\code{random}}{Takes some completion.}
      \item{\code{shortest}}{Takes the shortest completion in terms of
	characters.}
    }
  }
}
\value{
  A character vector with completed \code{words}.
}
\examples{
data("crude")
stemCompletion(crude, c("compan", "entit", "suppl"))
}
\author{Ingo Feinerer}
