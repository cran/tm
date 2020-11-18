\name{XMLTextDocument}
\alias{XMLTextDocument}
\title{XML Text Documents}
\description{
  Create \acronym{XML} text documents.
}
\usage{
XMLTextDocument(x = xml_missing(),
                author = character(0),
                datetimestamp = as.POSIXlt(Sys.time(), tz = "GMT"),
                description = character(0),
                heading = character(0),
                id = character(0),
                language = character(0),
                origin = character(0),
                \dots,
                meta = NULL)
}
\arguments{
  \item{x}{An \code{\link[xml2:read_xml]{XMLDocument}}.}
  \item{author}{a character or an object of class \code{\link{person}} giving
    the author names.}
  \item{datetimestamp}{an object of class \code{\link{POSIXt}} or a character
    string giving the creation date/time information.  If a character string,
    exactly one of the \acronym{ISO} 8601 formats defined by
    \url{https://www.w3.org/TR/NOTE-datetime} should be used.
    See \code{\link[NLP]{parse_ISO_8601_datetime}} in package \pkg{NLP}
    for processing such date/time information.
  }
  \item{description}{a character giving a description.}
  \item{heading}{a character giving the title or a short heading.}
  \item{id}{a character giving a unique identifier.}
  \item{language}{a character giving the language (preferably as \acronym{IETF}
    language tags, see \link[NLP]{language} in package \pkg{NLP}).}
  \item{origin}{a character giving information on the source and origin.}
  \item{\dots}{user-defined document metadata tag-value pairs.}
  \item{meta}{a named list or \code{NULL} (default) giving all metadata. If set
    all other metadata arguments are ignored.}
}
\value{
  An object inheriting from \code{XMLTextDocument} and
  \code{\link{TextDocument}}.
}
\seealso{
  \code{\link{TextDocument}} for basic information on the text document
  infrastructure employed by package \pkg{tm}.
}
\examples{
xml <- system.file("extdata", "order-doc.xml", package = "xml2")
(xtd <- XMLTextDocument(xml2::read_xml(xml),
                        heading = "XML text document",
                        id = xml,
                        language = "en"))
content(xtd)
meta(xtd)
}
