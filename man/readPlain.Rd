\name{readPlain}
\alias{readPlain}
\title{Read In a Text Document}
\description{
  Read in a text document without knowledge about its internal structure and
  possible available metadata.
}
\usage{
readPlain(elem, language, id)
}
\arguments{
  \item{elem}{a list with the named component \code{content} which must hold
    the document to be read in.}
  \item{language}{a string giving the text's language.}
  \item{id}{a unique identification string for the returned text document.}
}
\value{
  A \code{\link{PlainTextDocument}} representing \code{elem$content}.
}
\seealso{
  \code{\link{getReaders}} to list available reader functions.
}
\examples{
docs <- c("This is a text.", "This another one.")
vs <- VectorSource(docs)
elem <- getElem(stepNext(vs))
(result <- readPlain(elem, "en", "id1"))
meta(result)
}
\author{Ingo Feinerer}
