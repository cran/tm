\name{tm_map}
\alias{tm_map}
\alias{tm_map.VCorpus}
\alias{tm_map.PCorpus}
\title{Transformations on Corpora}
\description{
  Interface to apply transformation functions (also denoted as mappings)
  to corpora.
}
\usage{
\method{tm_map}{PCorpus}(x, FUN, \dots, useMeta = FALSE, lazy = FALSE)
\method{tm_map}{VCorpus}(x, FUN, \dots, useMeta = FALSE, lazy = FALSE)
}
\arguments{
  \item{x}{A corpus.}
  \item{FUN}{A transformation function returning a text document.}
  \item{\dots}{Arguments to \code{FUN}.}
  \item{useMeta}{Logical. Should \code{\link{DMetaData}} be passed over
    to \code{FUN} as argument?}
  \item{lazy}{Logical. Lazy mappings are mappings which are delayed
    until the documents' content is accessed. Lazy mapping is useful
    when working with large corpora but only few documents will be
    accessed, as it avoids the computationally expensive application of
    the mapping to all elements in the corpus.}
}
\value{
  A corpus with \code{FUN} applied to each document in \code{x}. In case
  of lazy mappings only annotations are stored which are evaluated upon
  access of individual documents which trigger the execution of the
  corresponding transformation function.
}
\seealso{
  \code{\link{getTransformations}} for available transformations, and
  \code{\link{materialize}} for manually triggering the materialization
  of documents with pending lazy transformations.
}
\note{
Please be aware that lazy transformations are an experimental feature and
change \R's standard evaluation semantics.
}
\examples{
data("crude")
tm_map(crude, stemDocument)
## Generate a custom transformation function which takes the heading
## as new content
headings <- function(x)
    PlainTextDocument(Heading(x), id = ID(x), language = Language(x))
inspect(tm_map(crude, headings))
}
