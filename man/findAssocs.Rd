\name{findAssocs}
\alias{findAssocs}
\alias{findAssocs.TermDocumentMatrix}
\alias{findAssocs.matrix}
\title{Find Associations in a Term-Document Matrix}
\description{
  Find associations in a term-document matrix.
}
\usage{
\method{findAssocs}{TermDocumentMatrix}(x, term, corlimit)
\method{findAssocs}{matrix}(x, term, corlimit)
}
\arguments{
  \item{x}{A term-document matrix.}
  \item{term}{A character holding a term.}
  \item{corlimit}{A numeric for the lower correlation bound limit.}
}
\value{A named numeric with those terms from \code{x} which correlate
  with \code{term} more than \code{corlimit}.}   
\examples{
data("crude")
tdm <- TermDocumentMatrix(crude)
findAssocs(tdm, "oil", 0.7)
}
