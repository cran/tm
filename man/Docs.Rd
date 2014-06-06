\name{Docs}
\alias{Docs}
\alias{nDocs}
\alias{nTerms}
\alias{Terms}
\title{Access Document IDs and Terms}
\description{
  Accessing document IDs, terms, and their number of a term-document matrix or
  document-term matrix.
}
\usage{
Docs(x)
nDocs(x)
nTerms(x)
Terms(x)
}
\arguments{
  \item{x}{Either a \code{\link{TermDocumentMatrix}} or
    \code{\link{DocumentTermMatrix}}.}
}
\value{
  For \code{Docs} and \code{Terms}, a character vector with document IDs and
  terms, respectively.

  For \code{nDocs} and \code{nTerms}, an integer with the number of document IDs
  and terms, respectively.
}
\examples{
data("crude")
tdm <- TermDocumentMatrix(crude)[1:10,1:20]
Docs(tdm)
nDocs(tdm)
nTerms(tdm)
Terms(tdm)
}
