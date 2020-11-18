\name{tm_filter}
\alias{tm_filter}
\alias{tm_filter.PCorpus}
\alias{tm_filter.SimpleCorpus}
\alias{tm_filter.VCorpus}
\alias{tm_index}
\alias{tm_index.PCorpus}
\alias{tm_index.SimpleCorpus}
\alias{tm_index.VCorpus}
\title{Filter and Index Functions on Corpora}
\description{
  Interface to apply filter and index functions to corpora.
}
\usage{
\method{tm_filter}{PCorpus}(x, FUN, \dots)
\method{tm_filter}{SimpleCorpus}(x, FUN, \dots)
\method{tm_filter}{VCorpus}(x, FUN, \dots)
\method{tm_index}{PCorpus}(x, FUN, \dots)
\method{tm_index}{SimpleCorpus}(x, FUN, \dots)
\method{tm_index}{VCorpus}(x, FUN, \dots)
}
\arguments{
  \item{x}{A corpus.}
  \item{FUN}{a filter function taking a text document or a string (if
    \code{x} is a \code{SimpleCorpus}) as input and returning the
    logical value \code{TRUE} or \code{FALSE}.}
  \item{\dots}{arguments to \code{FUN}.}
}
\value{
  \code{tm_filter} returns a corpus containing documents where
  \code{FUN} matches, whereas \code{tm_index} only returns the
  corresponding indices.
}
\examples{
data("crude")
# Full-text search
tm_filter(crude, FUN = function(x) any(grep("co[m]?pany", content(x))))
}
