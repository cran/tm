\name{preprocessReut21578XML}
\alias{preprocessReut21578XML}
\title{Preprocess the Reuters21578 XML archive.}
\description{
  Preprocess the Reuters21578 XML archive by correcting invalid UTF-8
  encodings and copying each text document into a separate file.
}
\usage{
preprocessReut21578XML(ReutersDir, ReutersOapfDir, fixEnc = TRUE)
}
\arguments{
  \item{ReutersDir}{a character describing the input directory.}
  \item{ReutersOapfDir}{a character describing the output directory.}
  \item{fixEnc}{a logical value indicating whether a invalid UTF-8
    encoding in the Reuters21578 XML dataset should be corrected.}
}
\value{
  No explicit return value. As a side product the directory
  \code{ReutersOapfDir} contains the corrected dataset.
}
\references{
  Lewis, David (1997) \emph{Reuters-21578 Text Categorization
    Collection Distribution 1.0}.
  \url{http://kdd.ics.uci.edu/databases/reuters21578/reuters21578.html}

  Luz, Saturnino \emph{XML-encoded version of Reuters-21578}.
  \url{http://modnlp.berlios.de/reuters21578.html}
}
\author{Ingo Feinerer}
\keyword{file}
