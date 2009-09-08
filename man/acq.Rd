\name{acq}
\docType{data}
\alias{acq}
\title{50 Exemplary News Articles from the Reuters-21578 XML Data Set of Topic acq}
\description{
  This dataset holds 50 news articles with additional meta
  information from the Reuters-21578 \acronym{XML} data set. All documents belong
  to the topic \code{acq} dealing with corporate acquisitions.
}
\usage{data("acq")}
\format{A corpus of 50 text documents.}
\source{Reuters-21578 Text Categorization Collection Distribution 1.0
  (\acronym{XML} format).}
\references{
  Lewis, David (1997) \emph{Reuters-21578 Text Categorization
    Collection Distribution 1.0}.
  \url{http://kdd.ics.uci.edu/databases/reuters21578/reuters21578.html}

  Luz, Saturnino \emph{\acronym{XML}-encoded version of Reuters-21578}.
  \url{http://modnlp.berlios.de/reuters21578.html}
}
\examples{
data("acq")
summary(acq)
}
\keyword{datasets}
