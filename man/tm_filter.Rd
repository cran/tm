\name{tm_filter}
\alias{tm_filter}
\alias{tm_filter.Corpus}
\alias{tm_index}
\alias{tm_index.Corpus}
\title{Filter and Index Functions on Corpora}
\description{
  Interface to apply filter and index functions to corpora.
}
\usage{
\method{tm_filter}{Corpus}(x, \dots, FUN = searchFullText,
          doclevel = TRUE, useMeta = FALSE)
\method{tm_index}{Corpus}(x, \dots, FUN = searchFullText,
          doclevel = TRUE, useMeta = FALSE)
}
\arguments{
  \item{x}{A corpus.}
  \item{\dots}{Arguments to \code{FUN}.}
  \item{FUN}{A filter function returning a logical value.} 
  \item{doclevel}{Logical. If the document level flag is set \code{FUN}
    is applied to each element of \code{x}, otherwise \code{FUN} is
    applied to \code{x} itself. If \code{FUN} has an attribute
    \code{doclevel} its value will be automatically used.}
  \item{useMeta}{Logical. Should \code{\link{DMetaData}} be passed over
    to \code{FUN} as argument?}
}
\value{
  \code{tm_filter} returns a corpus containing documents where
  \code{FUN} matches, whereas \code{tm_index} only returns the
  corresponding indices.
}
\seealso{
  \code{\link{sFilter}} for a filter using a simple statement query
  language, and \code{\link{getFilters}} to list available filter and
  index functions.
}
\examples{
data("crude")
attr(searchFullText, "doclevel")
tm_filter(crude, FUN = searchFullText, "company")
tm_index(crude, FUN = searchFullText, "company")
}
