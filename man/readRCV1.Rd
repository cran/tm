\name{readRCV1}
\alias{readRCV1}
\title{Read In a Reuters Corpus Volume 1 Document}
\description{
  Read in a Reuters Corpus Volume 1 XML document.
}
\usage{
readRCV1(elem, language, id)
}
\arguments{
  \item{elem}{A \code{list} with the two named elements \code{content}
    and \code{uri}. The first element holds the document to
    be read in, the second element holds a call to extract this
    document. The call is evaluated upon a request for load on demand.}
  \item{language}{A \code{character} vector giving the text's language.}
  \item{id}{A \code{character} vector representing a unique identification
    string for the returned text document.}
}
\value{
  An \code{XMLTextDocument}.
}
\seealso{
  Use \code{\link{getReaders}} to list available reader functions.
}
\author{Ingo Feinerer}
