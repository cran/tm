\name{readPDF}
\alias{readPDF}
\title{Read In a PDF Document}
\description{
  Return a function which reads in a portable document format (PDF)
  document extracting both its text and its meta data.
}
\usage{
readPDF(PdftotextOptions = "", \dots)
}
\arguments{
  \item{PdftotextOptions}{Options passed over to \command{pdftotext}.}
  \item{\dots}{Arguments for the generator function.}
}
\details{
  Formally this function is a function generator, i.e., it returns a
  function (which reads in a text document) with a well-defined
  signature, but can access passed over arguments (e.g., options to
  \command{pdftotext}) via lexical scoping.

  Note that this PDF reader needs the tool \command{pdftotext} installed
  and accessible on your system, available as command line utility in
  the Poppler PDF rendering library (see
  \url{http://poppler.freedesktop.org/}).
}
\value{
  A \code{function} with the signature \code{elem, language, id}:
  \item{elem}{A \code{list} with the named element \code{uri} of type
  \code{character} which must hold a valid file name.}
  \item{language}{A \code{character} vector giving the text's language.}
  \item{id}{A \code{character} vector representing a unique identification
    string for the returned text document.}
  The function returns a \code{PlainTextDocument} representing the text
  and meta data in \code{content}.
}
\seealso{
  \code{\link{getReaders}} to list available reader functions.
}
\examples{
if(file.exists(Sys.which("pdftotext"))) {
    f <- system.file("doc", "tm.pdf", package = "tm")
    pdf <- readPDF(PdftotextOptions = "-layout")(elem = list(uri = f),
                                                 language = "en",
                                                 id = "id1")
    pdf[1:13]
}
}
\author{Ingo Feinerer}
\keyword{file}
