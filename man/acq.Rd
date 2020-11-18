\name{acq}
\docType{data}
\alias{acq}
\title{50 Exemplary News Articles from the Reuters-21578 Data Set of Topic acq}
\description{
  This dataset holds 50 news articles with additional meta information from the
  Reuters-21578 data set. All documents belong to the topic \code{acq} dealing
  with corporate acquisitions.
}
\usage{data("acq")}
\format{A \code{\link{VCorpus}} of 50 text documents.}
\source{Reuters-21578 Text Categorization Collection Distribution 1.0
  (\acronym{XML} format).}
\references{
  Emms, Martin and Luz, Saturnino (2007).
  Machine Learning for Natural Language Processing.
  \emph{European Summer School of Logic, Language and Information,
    course reader}.
  % \url{http://ronaldo.cs.tcd.ie/esslli07/mlfornlp.pdf}
  \url{http://www.homepages.ed.ac.uk/sluzfil/esslli07/mlfornlp.pdf}

  Lewis, David (1997) \emph{Reuters-21578 Text Categorization
    Collection Distribution 1.0}.
  \url{http://kdd.ics.uci.edu/databases/reuters21578/reuters21578.html}

  Luz, Saturnino \emph{\acronym{XML}-encoded version of Reuters-21578}.
  % \url{http://ronaldo.cs.tcd.ie/esslli07/data/reuters21578-xml/}
  \url{http://www.homepages.ed.ac.uk/sluzfil/esslli07/data/reuters21578-xml.tar.bz2}
}
\examples{
data("acq")
acq
}
\keyword{datasets}
