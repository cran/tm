\name{VectorSource}
\alias{VectorSource}
\title{Vector Source}
\description{
  Create a vector source.
}
\usage{
VectorSource(x)
}
\arguments{
  \item{x}{A vector giving the texts.}
}
\details{
  A \emph{vector source} interprets each element of the vector \code{x} as a
  document.
}
\value{
  An object inheriting from \code{VectorSource}, \code{\link{SimpleSource}},
  and \code{\link{Source}}.
}
\seealso{
  \code{\link{Source}} for basic information on the source infrastructure
  employed by package \pkg{tm}.
}
\examples{
docs <- c("This is a text.", "This another one.")
(vs <- VectorSource(docs))
inspect(VCorpus(vs))
}
