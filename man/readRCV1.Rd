\name{readRCV1}
\alias{readRCV1}
\title{Read In a Reuters Corpus Volume 1 Document}
\description{
  Read in a Reuters Corpus Volume 1 \acronym{XML} document.
}
\usage{
readRCV1(elem, language, id)
}
\arguments{
  \item{elem}{A \code{list} with the two named elements \code{content}
    and \code{uri}. The first element holds the document to
    be read in, the second element holds a call to extract this
    document. The call is evaluated upon a request for load on demand.}
  \item{language}{A \code{character} vector giving the text's language.}
  \item{id}{A \code{character} vector representing a unique identification
    string for the returned text document.}
}
\value{
  An \code{RCV1Document}.
}
\seealso{
  Use \code{\link{getReaders}} to list available reader functions.
}
\references{
  Lewis, D. D.; Yang, Y.; Rose, T.; and Li, F (2004).
  RCV1: A New Benchmark Collection for Text Categorization Research.
  \emph{Journal of Machine Learning Research}, \bold{5}, 361--397.
  \url{http://www.jmlr.org/papers/volume5/lewis04a/lewis04a.pdf}
}
\examples{
f <- system.file("texts", "rcv1_2330.xml", package = "tm")
rcv1 <- readRCV1(elem = list(content = readLines(f)), language = "eng", id = "id1")
meta(rcv1)
}
\author{Ingo Feinerer}
