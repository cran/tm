\name{prescindMeta}
\alias{prescindMeta}
\title{Prescind meta data}
\description{
Extracts meta data from each individual document (either stored in its slots
or in additional user-defined local meta data pairs) of a corpus and
creates a dataframe which contains both the global meta data information
of the corpus plus the extracted (i.e., shifted up) local meta data.
}
\usage{
prescindMeta(object, meta)
}
\arguments{
  \item{object}{a corpus.}
  \item{meta}{a character vector of meta data names to be shifted up.}
}
\value{
  A data frame constructed from \code{object} with shifted up
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
