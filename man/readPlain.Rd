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
  \item{elem}{a named list with the component \code{content} which must hold
    the document to be read in.}
  \item{language}{a string giving the language.}
  \item{id}{a character giving a unique identifier for the created text
    document.}
}
\value{
  A \code{\link{PlainTextDocument}} representing \code{elem$content}. The
  argument \code{id} is used as fallback if \code{elem$uri} is null.
}
\seealso{
  \code{\link{Reader}} for basic information on the reader infrastructure
  employed by package \pkg{tm}.
}
\examples{
docs <- c("This is a text.", "This another one.")
vs <- VectorSource(docs)
elem <- getElem(stepNext(vs))
(result <- readPlain(elem, "en", "id1"))
meta(result)
}
