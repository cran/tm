\name{readDOC}
\alias{readDOC}
\title{Read In a MS Word Document}
\description{
  Return a function which reads in a Microsoft Word document extracting
  its text.
}
\usage{
readDOC(engine = c("antiword", "executable"), AntiwordOptions = "")
}
\arguments{
  \item{engine}{a character string for the preferred \acronym{DOC} extraction
    engine (see \bold{Details}).}
  \item{AntiwordOptions}{Options passed over to \command{antiword} executable.}
}
\details{
  Formally this function is a function generator, i.e., it returns a
  function (which reads in a text document) with a well-defined
  signature, but can access passed over arguments (e.g., options to
  \command{antiword}) via lexical scoping.

  Available \acronym{DOC} extraction engines are as follows.
  \describe{
    \item{\code{"antiword"}}{(default) Antiword utility as provided by the
      function \code{\link[antiword]{antiword}} in package \pkg{antiword}.}
    \item{\code{"executable"}}{command line \command{antiword} executable which
      must be installed and accessible on your system. This can convert
      documents from Microsoft Word version 2, 6, 7, 97, 2000, 2002 and 2003 to
      plain text, and is available from \url{http://www.winfield.demon.nl/}. The
      character vector \code{AntiwordOptions} is passed over to the executable.}
  }
}
\value{
  A \code{function} with the following formals:
  \describe{
    \item{\code{elem}}{a list with the named component \code{uri} which must
      hold a valid file name.}
    \item{\code{language}}{a string giving the language.}
    \item{\code{id}}{Not used.}
  }
  The function returns a \code{\link{PlainTextDocument}} representing the text
  and metadata extracted from \code{elem$uri}.
}
\seealso{
  \code{\link{Reader}} for basic information on the reader infrastructure
  employed by package \pkg{tm}.
}
