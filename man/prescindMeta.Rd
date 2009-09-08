\name{prescindMeta}
\alias{prescindMeta}
\title{Prescind Document Meta Data}
\description{
  Extracts meta data from each individual document (either stored in its attributes
  or in additional user-defined local meta data pairs) of a corpus and
  creates a data frame which contains both the global meta data information
  of the corpus plus the extracted (i.e., shifted up) local meta data of
  the individual text documents.
}
\usage{
prescindMeta(x, meta)
}
\arguments{
  \item{x}{A corpus.}
  \item{meta}{A character vector of meta data names to be shifted up.}
}
\value{
  A data frame constructed from \code{x} with shifted up
  \code{meta} data.
}
\seealso{
  \code{\link{DMetaData}}, and \code{\link{meta}}
}
\examples{
data("crude")
DMetaData(crude)
meta(crude, tag = "ID", type = "local")
prescindMeta(crude, c("ID", "Heading"))
}
