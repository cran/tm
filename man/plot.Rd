\name{plot}
\alias{plot.TermDocumentMatrix}
\title{Visualize a Term-Document Matrix}
\description{
  Visualize correlations between terms of a term-document matrix.
}
\usage{
\method{plot}{TermDocumentMatrix}(x,
     terms = sample(Terms(x), 20),
     corThreshold = 0.7,
     weighting = FALSE,
     attrs = list(graph = list(rankdir = "BT"),
                  node = list(shape = "rectangle",
                              fixedsize = FALSE)),
     \dots)
}
\arguments{
  \item{x}{A term-document matrix.}
  \item{terms}{Terms to be plotted. Defaults to 20 randomly chosen terms
    of the term-document matrix.}
  \item{corThreshold}{Do not plot correlations below this
    threshold. Defaults to \code{0.7}.}
  \item{weighting}{Define whether the line width corresponds to the
    correlation.}
  \item{attrs}{Argument passed to the plot method for class
    \code{\link[graph:graphNEL-class]{graphNEL}}.}
  \item{\dots}{Other arguments passed to the
    \code{\link[graph:graphNEL-class]{graphNEL}} plot method.}
}
\details{
  Visualization requires that package \pkg{Rgraphviz} is available.
}
\examples{
\dontrun{data(crude)
tdm <- TermDocumentMatrix(crude,
                          control = list(removePunctuation = TRUE,
                                         removeNumbers = TRUE,
                                         stopwords = TRUE))
plot(tdm, corThreshold = 0.2, weighting = TRUE)}
}
