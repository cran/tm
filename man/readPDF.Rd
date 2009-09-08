\name{readPDF}
\alias{readPDF}
\title{Read In a PDF Document}
\description{
  Return a function which reads in a portable document format (PDF)
  document extracting both its text and its meta data.
}
\usage{
readPDF(PdfinfoOptions = "", PdftotextOptions = "", \dots)
}
\arguments{
  \item{PdfinfoOptions}{Options passed over to \code{pdfinfo}.}
  \item{PdftotextOptions}{Options passed over to \code{pdftotext}.}
  \item{\dots}{Arguments for the generator function.}
}
\details{
  Formally this function is a function generator, i.e., it returns a
  function (which reads in a text document) with a well-defined
  signature, but can access passed over arguments (e.g., options to
  \code{pdfinfo} or \code{pdftotext}) via lexical scoping.

  Note that this PDF reader needs both the tools \code{pdftotext} and
  \code{pdfinfo} installed and accessible on your system.
}
\value{
  A \code{function} with the signature \code{elem, language, id}:
  \item{elem}{A \code{list} with the two named elements \code{content}
    and \code{uri}. The first element must hold the document to
    be read in, the second element must hold a call to extract this
    document. The call is evaluated upon a request for load on demand.}
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
f <- system.file("doc", "tm.pdf", package = "tm")
pdf <- readPDF(PdftotextOptions = "-layout")(elem = list(uri = f), language = "eng", id = "id1")
pdf[1:13]
}
\author{Ingo Feinerer}
