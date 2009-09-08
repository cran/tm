\name{inspect}
\alias{inspect}
\alias{inspect.PCorpus}
\alias{inspect.TermDocumentMatrix}
\alias{inspect.VCorpus}
\title{Inspect Objects}
\description{
  Inspect, i.e., display detailed information on a corpus or a
  term-document matrix.
}
\usage{
\method{inspect}{PCorpus}(x)
\method{inspect}{VCorpus}(x)
\method{inspect}{TermDocumentMatrix}(x)
}
\arguments{
  \item{x}{Either a corpus or a term-document matrix.}
}
\examples{
data("crude")
inspect(crude[1:3])
tdm <- TermDocumentMatrix(crude)[1:10, 1:10]
inspect(tdm)
}
