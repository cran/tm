\name{readGmane}
\alias{readGmane}
\title{Read In a Gmane RSS Feed}
\description{
  Read in a \acronym{RSS} feed as returned by Gmane for mailing lists.
}
\usage{
readGmane(elem, language, id)
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
  A \code{\link{PlainTextDocument}}.
}
\seealso{
  \code{\link{getReaders}} to list available reader functions.
}
\examples{
\dontrun{
gs <- GmaneSource(url("http://rss.gmane.org/gmane.comp.lang.r.general"))
elem <- getElem(stepNext(gs))
(gmane <- readGmane(elem, language = "en", id = "id1"))
meta(gmane)}
}
\author{Ingo Feinerer}
