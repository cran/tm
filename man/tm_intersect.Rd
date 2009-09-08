\name{tm_intersect}
\alias{tm_intersect}
\alias{tm_intersect.PlainTextDocument}
\title{Intersection between Documents and Words}
\description{
  Perform intersection on text documents and words.
}
\usage{
\method{tm_intersect}{PlainTextDocument}(x, y)
}
\arguments{
  \item{x}{A text document.}
  \item{y}{A character vector of words to be intersected with.}
}
\value{
  A logical value indicating whether a word in \code{y} appears in \code{x}.
}
\seealso{
  \code{\link{getFilters}} to list available filter functions.
}
\examples{
data("crude")
crude[[1]]
tm_intersect(crude[[1]], c("crude", "oil"))
tm_intersect(crude[[1]], "acquisition")
}
