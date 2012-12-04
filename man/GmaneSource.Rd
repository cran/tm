\name{GmaneSource}
\docType{methods}
\alias{GmaneSource}
\title{Gmane Source}
\description{
  Construct a source for a Gmane mailing list \acronym{RSS} feed.
}
\usage{
GmaneSource(x, encoding = "unknown")
}
\arguments{
  \item{x}{Either a character identifying the file or a connection.}
  \item{encoding}{encoding to be assumed for input strings.  It is used to mark
    character strings as known to be in Latin-1 or UTF-8: it is not used to
    re-encode the input.}
}
\value{
  An object of class \code{GmaneSource} which extends the class
  \code{Source} representing a Gmane mailing list \acronym{RSS} feed.
}
\author{Ingo Feinerer}
\seealso{
  \code{\link{getSources}} to list available sources. \code{\link{Encoding}} on encodings in \R.
}
\examples{
\dontrun{
GmaneSource(url("http://rss.gmane.org/gmane.comp.lang.r.general"))
}
}
