\name{TermDocMatrix}
\docType{methods}
\alias{TermDocMatrix}
\alias{TermDocMatrix,Corpus-method}
\alias{as.matrix,TermDocMatrix-method}
\title{Term-Document Matrix}
\description{
  Constructs a term-document matrix.
}
\usage{
TermDocMatrix(object, control = list())
}
\arguments{
  \item{object}{a text document collection}
  \item{control}{a named list of control options. The component
    \code{weighting} must be a weighting function capable of handling a
    \code{dgCMatrix}. It defaults to \code{weightTf} for term frequency
    weighting. All other options are delegated internally to a
    \code{\link{termFreq}} call.}
}
\value{
  An S4 object of class \code{TermDocMatrix} containing a sparse term-document
  matrix. The following slots contain useful information:

  \item{Data}{The sparse \code{Matrix}.}
  \item{Weighting}{The weighting mode applied to the term-document matrix.}
}
\seealso{
  The documentation to \code{\link{termFreq}} gives an extensive list of
  possible options.

  Available weighting functions shipped with this
  package are \code{\link{weightTf}}, \code{\link{weightTfIdf}},
  \code{\link{weightBin}} and \code{\link{weightLogical}}.
}
\examples{
data("crude")
(tdm <- TermDocMatrix(crude, control = list(weighting = weightTfIdf, stopwords = TRUE)))
}
\author{Ingo Feinerer}
\keyword{methods}
