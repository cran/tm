\name{PlainTextDocument}
\alias{PlainTextDocument}
\title{Plain Text Documents}
\description{
  Create plain text documents.
}
\usage{
PlainTextDocument(x = character(0),
                  author = character(0),
                  datetimestamp = as.POSIXlt(Sys.time(), tz = "GMT"),
                  description = character(0),
                  heading = character(0),
                  id = character(0),
                  language = character(0),
                  origin = character(0),
                  \dots,
                  meta = NULL,
                  class = NULL)
}
\arguments{
  \item{x}{A character string giving the plain text content.}
  \item{author}{a character string or an object of class \code{\link{person}} giving
    the author names.}
  \item{datetimestamp}{an object of class \code{\link{POSIXt}} or a character
    string giving the creation date/time information.  If a character string,
    exactly one of the \acronym{ISO} 8601 formats defined by
    \url{https://www.w3.org/TR/NOTE-datetime} should be used.
    See \code{\link[NLP]{parse_ISO_8601_datetime}} in package \pkg{NLP}
    for processing such date/time information.
  }
  \item{description}{a character string giving a description.}
  \item{heading}{a character string giving the title or a short heading.}
  \item{id}{a character string giving a unique identifier.}
  \item{language}{a character string giving the language (preferably as \acronym{IETF}
    language tags, see \link[NLP]{language} in package \pkg{NLP}).}
  \item{origin}{a character string giving information on the source and origin.}
  \item{\dots}{user-defined document metadata tag-value pairs.}
  \item{meta}{a named list or \code{NULL} (default) giving all metadata. If set
    all other metadata arguments are ignored.}
  \item{class}{a character vector or \code{NULL} (default) giving
    additional classes to be used for the created plain text document.}
}
\value{
  An object inheriting from \code{class}, \code{PlainTextDocument} and
  \code{\link{TextDocument}}.
}
\seealso{
  \code{\link{TextDocument}} for basic information on the text document
  infrastructure employed by package \pkg{tm}.
}
\examples{
(ptd <- PlainTextDocument("A simple plain text document",
                          heading = "Plain text document",
                          id = basename(tempfile()),
                          language = "en"))
meta(ptd)
}
