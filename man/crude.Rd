\name{crude}
\docType{data}
\alias{crude}
\title{20 Exemplary News Articles from the Reuters-21578 Data Set of Topic crude}
\description{
  This data set holds 20 news articles with additional meta information from
  the Reuters-21578 data set. All documents belong to the topic \code{crude}
  dealing with crude oil.
}
\usage{data("crude")}
\format{A \code{\link{VCorpus}} of 20 text documents.}
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
data("crude")
crude
}
\keyword{datasets}
