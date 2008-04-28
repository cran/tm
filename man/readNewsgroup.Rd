\name{readNewsgroup}
\alias{readNewsgroup}
\title{Read In a Newsgroup Document}
\description{
  Returns a function which reads in a newsgroup document as found in the
  \acronym{UCI} \acronym{KDD} newsgroup data set.
}
\usage{
readNewsgroup(DateFormat = "\%d \%B \%Y \%H:\%M:\%S", ...)
}
\arguments{
  \item{DateFormat}{the format of the Date header in the newsgroup document.}
  \item{\dots}{arguments for the generator function.}
}
\details{
  Formally this function is a function generator, i.e., it returns a
  function (which reads in a newsgroup document) with a well-defined
  signature, but can access passed over arguments (e.g., to specify the
  format of the Date header in the newsgroup document via
  \code{DateFormat}) via lexical scoping.
}
\value{
  A \code{function} with the signature \code{elem, language, load, id}:
  
  \item{elem}{A \code{list} with the two named elements \code{content}
    and \code{uri}. The first element must hold the document to
    be read in, the second element must hold a call to extract this
    document. The call is evaluated upon a request for load on demand.}
  \item{language}{A \code{character} vector giving the text's language.}
  \item{load}{A \code{logical} value indicating whether the document
    corpus should be immediately loaded into memory.}
  \item{id}{A \code{character} vector representing a unique identification
    string for the returned text document.}

  The function returns a \code{NewsgroupDocument} representing
  \code{content}.
}
\seealso{
  Use \code{\link{getReaders}} to list available reader functions.

  See \code{\link{strptime}} for date format specifications.
}
\author{Ingo Feinerer}
\keyword{file}
