\name{convertReut21578XMLPlain}
\alias{convertReut21578XMLPlain}
\title{Transform A Reuters21578 XML Document To A Plain Text Document}
\description{
  Transform a Reuters21578 XML document to a plain text
  document.
}
\usage{
convertReut21578XMLPlain(node, ...)
}
\arguments{
  \item{node}{an XML node representing a <REUTERS></REUTERS> element
    from a well-formed Reuters-21578 XML file.}
  \item{...}{Arguments passed over by calling functions.}
}
\value{
  A \code{PlainTextDocument} representing \code{node}.
}
\seealso{\code{\link{asPlain}}}
\author{Ingo Feinerer}
\keyword{file}
