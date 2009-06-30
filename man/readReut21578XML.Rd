\name{readReut21578XML}
\alias{readReut21578XML}
\alias{readReut21578XMLasPlain}
\title{Read In a Reuters21578 XML Document}
\description{
  Read in a Reuters21578 XML document.
}
\usage{
readReut21578XML(elem, language, id)
readReut21578XMLasPlain(elem, language, id)
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
   An \code{XMLTextDocument} for \code{readReut21578XML}, or a
   \code{PlainTextDocument} for \code{readReut21578XMLasPlain}.
}
\seealso{
  Use \code{\link{getReaders}} to list available reader functions.
}
\author{Ingo Feinerer}
