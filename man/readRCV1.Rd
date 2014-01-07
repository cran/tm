\name{readRCV1}
\alias{readRCV1}
\alias{readRCV1asPlain}
\title{Read In a Reuters Corpus Volume 1 Document}
\description{
  Read in a Reuters Corpus Volume 1 \acronym{XML} document.
}
\usage{
readRCV1(elem, language, id)
readRCV1asPlain(elem, language, id)
}
\arguments{
  \item{elem}{a list with the named component \code{content} which must hold
    the document to be read in.}
  \item{language}{a string giving the text's language.}
  \item{id}{a unique identification string for the returned text document.}
}
\value{
  An \code{\link{RCV1Document}} for \code{readRCV1}, or a
  \code{\link{PlainTextDocument}} for \code{readRCV1asPlain}.
}
\seealso{
  \code{\link{getReaders}} to list available reader functions.
}
\references{
  Lewis, D. D.; Yang, Y.; Rose, T.; and Li, F (2004).
  RCV1: A New Benchmark Collection for Text Categorization Research.
  \emph{Journal of Machine Learning Research}, \bold{5}, 361--397.
  \url{http://www.jmlr.org/papers/volume5/lewis04a/lewis04a.pdf}
}
\examples{
f <- system.file("texts", "rcv1_2330.xml", package = "tm")
rcv1 <- readRCV1(elem = list(content = readLines(f)),
                 language = "en", id = "id1")
meta(rcv1)
}
\author{Ingo Feinerer}
