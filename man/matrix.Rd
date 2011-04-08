\name{TermDocumentMatrix}
\alias{TermDocumentMatrix}
\alias{DocumentTermMatrix}
\alias{as.TermDocumentMatrix}
\alias{as.DocumentTermMatrix}
\title{Term-Document Matrix}
\description{
  Constructs or coerces to a term-document matrix or a document-term matrix.
}
\usage{
TermDocumentMatrix(x, control = list())
DocumentTermMatrix(x, control = list())
as.TermDocumentMatrix(x, \dots)
as.DocumentTermMatrix(x, \dots)
}
\arguments{
  \item{x}{a corpus for the constructors and either a term-document
    matrix or a document-term matrix or a \link[slam:matrix]{simple
    triplet matrix} (package \pkg{slam}) for the coercing functions.}
  \item{control}{a named list of control options. The component
    \code{weighting} must be a weighting function capable of handling a
    \code{TermDocumentMatrix}. It defaults to \code{weightTf} for term
    frequency weighting. All other options are delegated internally to a
    \code{\link{termFreq}} call.}
  \item{\dots}{the additional argument \code{weighting} (typically a
    \code{\link{WeightFunction}}) is allowed when coercing a
    simple triplet matrix to a term-document or document-term matrix.}
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
tdm <- TermDocumentMatrix(crude,
                          control = list(weighting = weightTfIdf,
                                         stopwords = TRUE))
dtm <- DocumentTermMatrix(crude,
                          control = list(weighting =
                                         function(x)
                                         weightTfIdf(x, normalize =
                                                     FALSE),
                                         stopwords = TRUE))
inspect(tdm[155:160,1:5])
inspect(dtm[1:5,155:160])
}
\author{Ingo Feinerer}
