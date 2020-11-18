\name{readPDF}
\alias{readPDF}
\title{Read In a PDF Document}
\description{
  Return a function which reads in a portable document format (\acronym{PDF})
  document extracting both its text and its metadata.
}
\usage{
readPDF(engine = c("pdftools", "xpdf", "Rpoppler",
                   "ghostscript", "Rcampdf", "custom"),
        control = list(info = NULL, text = NULL))
}
\arguments{
  \item{engine}{a character string for the preferred \acronym{PDF} extraction
    engine (see \bold{Details}).}
  \item{control}{a list of control options for the engine with the named
    components \code{info} and \code{text} (see \bold{Details}).}
}
\details{
  Formally this function is a function generator, i.e., it returns a function
  (which reads in a text document) with a well-defined signature, but can access
  passed over arguments (e.g., the preferred \acronym{PDF} extraction
  \code{engine} and \code{control} options) via lexical scoping.

  Available \acronym{PDF} extraction engines are as follows.
  \describe{
    \item{\code{"pdftools"}}{(default) Poppler \acronym{PDF} rendering library
      as provided by the functions \code{\link[pdftools]{pdf_info}} and
      \code{\link[pdftools]{pdf_text}} in package \pkg{pdftools}.}
    \item{\code{"xpdf"}}{command line \command{pdfinfo} and
      \command{pdftotext} executables which must be installed and accessible on
      your system. Suitable utilities are provided by the Xpdf
      (\url{http://www.xpdfreader.com/}) \acronym{PDF} viewer or by the
      Poppler (\url{https://poppler.freedesktop.org/}) \acronym{PDF} rendering
      library.}
    \item{\code{"Rpoppler"}}{Poppler \acronym{PDF} rendering library as
      provided by the functions \code{\link[Rpoppler]{PDF_info}} and
      \code{\link[Rpoppler]{PDF_text}} in package \pkg{Rpoppler}.}
    \item{\code{"ghostscript"}}{Ghostscript using \file{pdf_info.ps} and
      \file{ps2ascii.ps}.}
    \item{\code{"Rcampdf"}}{Perl CAM::PDF \acronym{PDF} manipulation library
      as provided by the functions \code{pdf_info} and \code{pdf_text}
      in package \pkg{Rcampdf}, available from the repository at
      \url{http://datacube.wu.ac.at}.}
    \item{\code{"custom"}}{custom user-provided extraction engine.}
  }

  Control parameters for engine \code{"xpdf"} are as follows.
  \describe{
    \item{\code{info}}{a character vector specifying options passed over to
      the \command{pdfinfo} executable.}
    \item{\code{text}}{a character vector specifying options passed over to
      the \command{pdftotext} executable.}
  }

  Control parameters for engine \code{"custom"} are as follows.
  \describe{
    \item{\code{info}}{a function extracting metadata from a \acronym{PDF}.
      The function must accept a file path as first argument and must return a
      named list with the components \code{Author} (as character string),
      \code{CreationDate} (of class \code{POSIXlt}), \code{Subject} (as
      character string), \code{Title} (as character string), and \code{Creator}
      (as character string).}
    \item{\code{text}}{a function extracting content from a \acronym{PDF}.
      The function must accept a file path as first argument and must return a
      character vector.}
  }
}
\value{
  A \code{function} with the following formals:
  \describe{
    \item{\code{elem}}{a named list with the component \code{uri} which must
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
\examples{
uri <- paste0("file://",
              system.file(file.path("doc", "tm.pdf"), package = "tm"))
engine <- if(nzchar(system.file(package = "pdftools"))) {
    "pdftools" 
} else {
    "ghostscript"
}
reader <- readPDF(engine)
pdf <- reader(elem = list(uri = uri), language = "en", id = "id1")
cat(content(pdf)[1])
VCorpus(URISource(uri, mode = ""),
        readerControl = list(reader = readPDF(engine = "ghostscript")))
}
\keyword{file}
