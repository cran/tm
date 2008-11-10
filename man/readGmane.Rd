\name{readGmane}
\alias{readGmane}
\title{Read In A Newsgroup Document}
\description{
  Returns a function which reads in a RSS feed as returned by Gmane for
  mailing lists.
}
\usage{
readGmane(...)
}
\arguments{
  \item{...}{arguments for the generator function.}
}
\details{
  Formally this function is a function generator, i.e., it returns a
  function (which reads in an RSS feed) with a well-defined signature,
  but can access passed over arguments via lexical scoping.  This is
  especially useful for reader functions for complex data structures
  which need a lot of configuration options.
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
}
\author{Ingo Feinerer}
\keyword{file}
