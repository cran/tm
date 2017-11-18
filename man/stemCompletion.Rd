\name{stemCompletion}
\alias{stemCompletion}
\title{Complete Stems}
\description{
  Heuristically complete stemmed words.
}
\usage{
stemCompletion(x,
               dictionary,
               type = c("prevalent", "first", "longest",
                        "none", "random", "shortest"))
}
\arguments{
  \item{x}{A character vector of stems to be completed.}
  \item{dictionary}{A \code{\link{Corpus}} or character vector to be searched
    for possible completions.}
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
  A character vector with completed words.
}
\examples{
data("crude")
stemCompletion(c("compan", "entit", "suppl"), crude)
}
\references{
  Ingo Feinerer (2010).
  Analysis and Algorithms for Stemming Inversion.
  \emph{Information Retrieval Technology --- 6th Asia Information Retrieval Societies Conference, AIRS 2010, Taipei, Taiwan, December 1--3, 2010. Proceedings}, volume 6458 of \emph{Lecture Notes in Computer Science}, pages 290--299. Springer-Verlag, December 2010.
}
