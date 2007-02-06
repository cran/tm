\name{plot}
\alias{plot.TermDocMatrix}
\title{Visualize Term-Document Matrix}
\description{
  Visualize correlations between terms of a term-document matrix.
}
\usage{
\method{plot}{TermDocMatrix}(x,
     terms = sample(colnames(x), 20),
     corThreshold = 0.7,
     weighting = FALSE,
     attrs = list(graph = list(rankdir = "BT"),
                  node = list(shape = "rectangle",
                              fixedsize = FALSE)),
     ...)
}
\arguments{
  \item{x}{an \R object inheriting from S4 class
    \code{\link{TermDocMatrix}}.}
  \item{terms}{Terms to be plotted. Defaults to 20 randomly chosen terms
    of the term-document matrix.}
  \item{corThreshold}{Do not plot correlations below this
    threshold. Defaults to \code{0.7}.}
  \item{weighting}{Define whether the line width corresponds to the
    correlation.}
  \item{attrs}{argument passed to the plot method for class
    \code{\link[graph:graphNEL-class]{graphNEL}}.}
  \item{\dots}{Other arguments passed to the
    \code{\link[graph:graphNEL-class]{graphNEL}} plot method.}
}
\details{
  Visualization requires that package \pkg{Rgraphviz} is available.
}
\examples{
if (require("Rgraphviz")) {
  data(crude)
  tdm <- TermDocMatrix(crude)
  plot(tdm, terms = colnames(tdm)[1:20])
  set.seed(1234)
  plot(tdm, corThreshold = 0.2, weighting = TRUE)
}
}
\keyword{math}
