\name{content_transformer}
\alias{content_transformer}
\title{Content Transformers}
\description{
  Create content transformers, i.e., functions which modify the content of an
  \R object.
}
\usage{
content_transformer(FUN)
}
\arguments{
  \item{FUN}{a function.}
}
\value{
  A function with two arguments:
  \describe{
    \item{\code{x}}{an \R object with implemented content getter
      (\code{\link[NLP]{content}}) and setter (\code{\link{content<-}})
      functions.}
    \item{\code{\dots}}{arguments passed over to \code{FUN}.}
  }
}
\seealso{
  \code{\link{tm_map}} for an interface to apply transformations to corpora.
}
\examples{
data("crude")
crude[[1]]
(f <- content_transformer(function(x, pattern) gsub(pattern, "", x)))
tm_map(crude, f, "[[:digit:]]+")[[1]]
}
