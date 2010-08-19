\name{RCV1Document}
\alias{RCV1Document}
\title{RCV1 Text Document}
\description{
  Construct an object representing a RCV1 XML text document with meta
  information.
}
\usage{
RCV1Document(x, author = character(0),
             datetimestamp = as.POSIXlt(Sys.time(), tz = "GMT"),
             description = character(0), heading = character(0),
             id = character(0), origin = character(0),
             language = character(0), localmetadata = list())
}
\arguments{
  \item{x}{Object of class \code{list} containing the content.}
  \item{author}{Object of class \code{character} containing
    the author names.}
  \item{datetimestamp}{Object of class \code{POSIXlt} containing the
    date and time when the document was written.}
  \item{description}{Object of class \code{character} containing
    additional text information.}
  \item{heading}{Object of class \code{character} containing
    the title or a short heading.}
  \item{id}{Object of class \code{character} containing
    an identifier.}
  \item{origin}{Object of class \code{character} containing
    information on the source and origin of the text.}
  \item{language}{Object of class \code{character} containing
    the language of the text (preferably as \acronym{IETF} language
    tags).}
  \item{localmetadata}{Object of class \code{list} containing
    local meta data in form of tag-value pairs.}
}
\seealso{
  \code{\link{PlainTextDocument}} and \code{\link{Reuters21578Document}}
}
\references{
  Lewis, D. D.; Yang, Y.; Rose, T.; and Li, F (2004).
  RCV1: A New Benchmark Collection for Text Categorization Research.
  \emph{Journal of Machine Learning Research}, \bold{5}, 361--397.
  \url{http://www.jmlr.org/papers/volume5/lewis04a/lewis04a.pdf}
}
\author{Ingo Feinerer}
