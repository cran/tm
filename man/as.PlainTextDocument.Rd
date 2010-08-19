\name{as.PlainTextDocument}
\alias{as.PlainTextDocument}
\alias{as.PlainTextDocument.PlainTextDocument}
\alias{as.PlainTextDocument.Reuters21578Document}
\alias{as.PlainTextDocument.RCV1Document}
\title{Create Objects of Class PlainTextDocument}
\description{
  Create objects of class PlainTextDocument.
}
\usage{
\method{as.PlainTextDocument}{PlainTextDocument}(x)
\method{as.PlainTextDocument}{Reuters21578Document}(x)
\method{as.PlainTextDocument}{RCV1Document}(x)
}
\arguments{
  \item{x}{A text document.}
}
\seealso{
  \code{\link{getTransformations}} to list available transformation (mapping) functions.
}
\examples{
reut21578 <- system.file("texts", "crude", package = "tm")
r <- Corpus(DirSource(reut21578),
                      readerControl = list(reader = readReut21578XML))
r[[1]]
as.PlainTextDocument(r[[1]])
}
