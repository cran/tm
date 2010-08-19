\name{GmaneSource}
\docType{methods}
\alias{GmaneSource}
\title{Gmane Source}
\description{
  Construct a source for a Gmane mailing list \acronym{RSS} feed.
}
\usage{
GmaneSource(x, encoding = "UTF-8")
}
\arguments{
  \item{x}{Either a character identifying the file or a connection.}
  \item{encoding}{A character giving the encoding of \code{x}.}
}
\value{
  An object of class \code{GmaneSource} which extends the class
  \code{Source} representing a Gmane mailing list \acronym{RSS} feed.
}
\author{Ingo Feinerer}
\seealso{
  \code{\link{getSources}} to list available sources.
}
\examples{
\dontrun{
GmaneSource(url("http://rss.gmane.org/gmane.comp.lang.r.general"))
}
}
