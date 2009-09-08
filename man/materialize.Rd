\name{materialize}
\alias{materialize}
\title{Materialize Lazy Mappings}
\description{
  The function \code{\link{tm_map}} supports so-called lazy mappings, that
  are mappings which are delayed until the documents' content is
  accessed. This function triggers the evaluation, i.e., it materializes
  the documents.
}
\usage{
materialize(corpus, range = seq_along(corpus))
}
\arguments{
  \item{corpus}{A document collection with lazy mappings.}
  \item{range}{The indices of documents to be materialized.}
}
\value{
  A corpus with materialized, i.e., all mappings computed and applied,
  documents for the requested range.
}
\author{Ingo Feinerer}
\seealso{\code{\link{tm_map}}}
\examples{
data("crude")
x <- tm_map(crude, stemDocument, lazy = TRUE)
x <- materialize(x)
}
