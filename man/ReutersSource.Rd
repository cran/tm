\name{ReutersSource}
\alias{ReutersSource}
\title{Reuters-21578 XML Source}
\description{
  Construct a source for an input containing several Reuters-21578 \acronym{XML}
  documents.
}
\usage{
ReutersSource(x, encoding = "unknown")
}
\arguments{
  \item{x}{Either a character identifying the file or a connection.}
  \item{encoding}{encoding to be assumed for input strings.  It is used to mark
    character strings as known to be in Latin-1 or UTF-8: it is not used to
    re-encode the input.}
}
\value{
  An object of class \code{XMLSource} which extends the class
  \code{Source} representing a Reuters-21578 \acronym{XML} document.
}
\references{
  Lewis, David (1997) \emph{Reuters-21578 Text Categorization Collection Distribution 1.0}.
  \url{http://kdd.ics.uci.edu/databases/reuters21578/reuters21578.html}

  Luz, Saturnino \emph{\acronym{XML}-encoded version of Reuters-21578}.
  \url{http://modnlp.berlios.de/reuters21578.html}
}
\author{Ingo Feinerer}
\seealso{
  \code{\link{getSources}} to list available sources. \code{\link{Encoding}} on encodings in \R.
}
\examples{
reuters21578 <- system.file("texts", "reuters-21578.xml", package = "tm")
rs <- ReutersSource(reuters21578)
inspect(Corpus(rs)[1:2])
}
