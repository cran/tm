\name{TermDocumentMatrix}
\alias{TermDocMatrix}
\alias{TermDocumentMatrix}
\alias{DocumentTermMatrix}
\title{Term-Document Matrix}
\description{
  Constructs a term-document matrix or a document-term matrix.
}
\usage{
TermDocumentMatrix(x, control = list())
DocumentTermMatrix(x, control = list())
}
\arguments{
  \item{x}{a corpus}
  \item{control}{a named list of control options. The component
    \code{weighting} must be a weighting function capable of handling a
    \code{TermDocumentMatrix}. It defaults to \code{weightTf} for term
    frequency weighting. All other options are delegated internally to a
    \code{\link{termFreq}} call.}
}
\value{
  An object of class \code{TermDocumentMatrix} or class
  \code{DocumentTermMatrix} containing a sparse term-document matrix or
  document-term matrix. The attribute \code{Weighting} contains the
  weighting applied to the matrix.
}
\seealso{
  The documentation of \code{\link{termFreq}} gives an extensive list of
  possible options.

  Available weighting functions shipped with the \pkg{tm}
  package are \code{\link{weightTf}}, \code{\link{weightTfIdf}}, and
  \code{\link{weightBin}}.
}
\examples{
data("crude")
tdm <- TermDocumentMatrix(crude, control = list(weighting = weightTfIdf, stopwords = TRUE))
dtm <- DocumentTermMatrix(crude, control = list(weighting = weightTfIdf, stopwords = TRUE))
inspect(tdm[165:170,1:5])
inspect(dtm[1:5,165:170])
}
\author{Ingo Feinerer}
