\name{convertRCV1Plain}
\alias{convertRCV1Plain}
\title{Transform A RCV1 Document To A Plain Text Document}
\description{
  Transform a Reuters Corpus Volume 1 XML document to a plain text
  document.
}
\usage{
convertRCV1Plain(node, ...)
}
\arguments{
  \item{node}{an XML node representing a <newsitem></newsitem> element
    from a well-formed RCV1 XML file.}
  \item{...}{Arguments passed over by calling functions.}
}
\value{
  A \code{PlainTextDocument} representing \code{node}.
}
\seealso{\code{\link{asPlain}}}
\examples{
rcv1 <- system.file("texts", "rcv1", package = "tm")
rcv1TDC <- TextDocCol(DirSource(rcv1), readerControl = list(reader = readRCV1, language = "en_US", load = TRUE))
rcv1TDC[[1]]
asPlain(rcv1TDC[[1]], convertRCV1Plain)
}
\author{Ingo Feinerer}
\keyword{file}
