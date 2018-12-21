\name{DataframeSource}
\alias{DataframeSource}
\title{Data Frame Source}
\description{
  Create a data frame source.
}
\usage{
DataframeSource(x)
}
\arguments{
  \item{x}{A data frame giving the texts and metadata.}
}
\details{
  A \emph{data frame source} interprets each row of the data frame \code{x} as a
  document. The first column must be named \code{"doc_id"} and contain a unique
  string identifier for each document. The second column must be named
  \code{"text"} and contain a UTF-8 encoded string representing the
  document's content. Optional additional columns are used as document level
  metadata.
}
\value{
  An object inheriting from \code{DataframeSource}, \code{\link{SimpleSource}},
  and \code{\link{Source}}.
}
\seealso{
  \code{\link{Source}} for basic information on the source infrastructure
  employed by package \pkg{tm}, and \code{\link{meta}} for types of metadata.

  \code{\link[readtext]{readtext}} for reading in a text in multiple formats
  suitable to be processed by \code{DataframeSource}.
}
\examples{
docs <- data.frame(doc_id = c("doc_1", "doc_2"),
                   text = c("This is a text.", "This another one."),
                   dmeta1 = 1:2, dmeta2 = letters[1:2],
                   stringsAsFactors = FALSE)
(ds <- DataframeSource(docs))
x <- Corpus(ds)
inspect(x)
meta(x)
}
