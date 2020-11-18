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
  \item{elem}{a named list with the component \code{content} which must hold
    the document to be read in.}
  \item{language}{a string giving the language.}
  \item{id}{Not used.}
}
\value{
  An \code{\link{XMLTextDocument}} for \code{readRCV1}, or a
  \code{\link{PlainTextDocument}} for \code{readRCV1asPlain}, representing the
  text and metadata extracted from \code{elem$content}.
}
\seealso{
  \code{\link{Reader}} for basic information on the reader infrastructure
  employed by package \pkg{tm}.
}
\references{
  Lewis, D. D.; Yang, Y.; Rose, T.; and Li, F (2004).
  RCV1: A New Benchmark Collection for Text Categorization Research.
  \emph{Journal of Machine Learning Research}, \bold{5}, 361--397.
  \url{https://www.jmlr.org/papers/volume5/lewis04a/lewis04a.pdf}
}
\examples{
f <- system.file("texts", "rcv1_2330.xml", package = "tm")
f_bin <- readBin(f, raw(), file.size(f))
rcv1 <- readRCV1(elem = list(content = f_bin), language = "en", id = "id1")
content(rcv1)
meta(rcv1)
}
