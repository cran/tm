\name{PlainTextDocument}
\alias{PlainTextDocument}
\title{Plain Text Document}
\description{
  Construct an object representing a plain text document with additional
  meta information.
}
\usage{
PlainTextDocument(x = character(0), author = character(0),
                  datetimestamp = as.POSIXlt(Sys.time(), tz = "GMT"),
                  description = character(0), heading = character(0),
                  id = character(0), origin = character(0),
                  language = character(0), localmetadata = list())
}
\arguments{
  \item{x}{Object of class \code{character} containing the
    content.}
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
\author{Ingo Feinerer}
