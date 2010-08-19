\name{Reuters21578Document}
\alias{Reuters21578Document}
\title{Reuters-21578 Text Document}
\description{
  Construct an object representing a Reuters-21578 XML text document
  with meta information.
}
\usage{
Reuters21578Document(x, author = character(0),
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
  \code{\link{PlainTextDocument}} and \code{\link{RCV1Document}}
}
\references{
  Lewis, David (1997) \emph{Reuters-21578 Text Categorization Collection Distribution 1.0}.
  \url{http://kdd.ics.uci.edu/databases/reuters21578/reuters21578.html}

  Luz, Saturnino \emph{\acronym{XML}-encoded version of Reuters-21578}.
  \url{http://modnlp.berlios.de/reuters21578.html}
}
\author{Ingo Feinerer}
