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
  \item{x}{A data frame giving the texts.}
}
\details{
  A \emph{data frame source} interprets each row of the data frame \code{x} as a
  document.
}
\value{
  An object inheriting from \code{DataframeSource}, \code{\link{SimpleSource}},
  and \code{\link{Source}}.
}
\seealso{
  \code{\link{Source}} for basic information on the source infrastructure
  employed by package \pkg{tm}.
}
\examples{
docs <- data.frame(c("This is a text.", "This another one."))
(ds <- DataframeSource(docs))
inspect(VCorpus(ds))
}
