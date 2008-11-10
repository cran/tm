\name{convertRCV1Plain}
\alias{convertRCV1Plain}
\title{Transform a RCV1 Document to a Plain Text Document}
\description{
  Transform a Reuters Corpus Volume 1 \acronym{XML} document to a plain text
  document.
}
\usage{
convertRCV1Plain(node, ...)
}
\arguments{
  \item{node}{A \code{RCV1Document} representing a well-formed RCV1 \acronym{XML} file.}
  \item{...}{Arguments passed over by calling functions.}
}
\value{
  A \code{PlainTextDocument} representing \code{node}.
}
\seealso{\code{\link{asPlain}}}
\examples{
rcv1 <- system.file("texts", "rcv1", package = "tm")
rcv1TDC <- Corpus(DirSource(rcv1), readerControl = list(reader = readRCV1))
rcv1TDC[[1]]
asPlain(rcv1TDC[[1]], convertRCV1Plain)
}
\author{Ingo Feinerer}
\keyword{file}
