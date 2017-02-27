\name{inspect}
\alias{inspect}
\alias{inspect.PCorpus}
\alias{inspect.TermDocumentMatrix}
\alias{inspect.TextDocument}
\alias{inspect.VCorpus}
\title{Inspect Objects}
\description{
  Inspect, i.e., display detailed information on a corpus, a
  term-document matrix, or a text document.
}
\usage{
\method{inspect}{PCorpus}(x)
\method{inspect}{VCorpus}(x)
\method{inspect}{TermDocumentMatrix}(x)
\method{inspect}{TextDocument}(x)
}
\arguments{
  \item{x}{Either a corpus, a term-document matrix, or a text document.}
}
\examples{
data("crude")
inspect(crude[1:3])
inspect(crude[[1]])
tdm <- TermDocumentMatrix(crude)[1:10, 1:10]
inspect(tdm)
}
