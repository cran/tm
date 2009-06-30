\name{convertReut21578XMLPlain}
\alias{convertReut21578XMLPlain}
\title{Transform a Reuters21578 XML Document to a Plain Text Document}
\description{
  Transform a Reuters21578 XML document to a plain text
  document.
}
\usage{
convertReut21578XMLPlain(node, \dots)
}
\arguments{
  \item{node}{an XML node representing a <REUTERS></REUTERS> element
    from a well-formed Reuters-21578 XML file.}
  \item{\dots}{Arguments passed over by calling functions.}
}
\value{
  A \code{PlainTextDocument} representing \code{node}.
}
\seealso{\code{\link{asPlain}}}
\examples{
reut21578 <- system.file("texts", "reut21578", package = "tm")
reut21578TDC <- Corpus(DirSource(reut21578), readerControl = list(reader = readReut21578XML))
reut21578TDC[[1]]
asPlain(reut21578TDC[[1]], convertReut21578XMLPlain)
}
\author{Ingo Feinerer}
