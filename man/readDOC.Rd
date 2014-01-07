\name{readDOC}
\alias{readDOC}
\title{Read In a MS Word Document}
\description{
  Return a function which reads in a Microsoft Word document extracting
  its text.
}
\usage{
readDOC(AntiwordOptions = "")
}
\arguments{
  \item{AntiwordOptions}{Options passed over to \command{antiword}.}
}
\details{
  Formally this function is a function generator, i.e., it returns a
  function (which reads in a text document) with a well-defined
  signature, but can access passed over arguments (e.g., options to
  \command{antiword}) via lexical scoping.

  Note that this MS Word reader needs the tool \command{antiword}
  installed and accessible on your system.  This can convert documents
  from Microsoft Word version 2, 6, 7, 97, 2000, 2002 and 2003 to plain
  text, and is available from \url{http://www.winfield.demon.nl/}.
}
\value{
  A \code{function} with the signature \code{elem, language, id}:
  \describe{
    \item{\code{elem}}{a list with the named component \code{uri} which must
      hold a valid file name.}
    \item{\code{language}}{a string giving the text's language.}
    \item{\code{id}}{a unique identification string for the returned text
      document.}
  }
  The function returns a \code{\link{PlainTextDocument}} representing the text
  and meta data extracted from \code{elem$uri}.
}
\seealso{
  \code{\link{getReaders}} to list available reader functions.
}
\author{Ingo Feinerer}
