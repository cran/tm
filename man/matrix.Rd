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
    triplet matrix} (package \pkg{slam}) or a \link[=termFreq]{term
    frequency vector} for the coercing functions.}
  \item{control}{a named list of control options. There are local
    options which are evaluated for each document and global options
    which are evaluated once for the constructed matrix. Available local
    options are documented in \code{\link{termFreq}} and are internally
    delegated to a \code{\link{termFreq}} call. Available global options
    are:
    \describe{
      \item{\code{bounds}}{A list with a tag \code{global} whose value
	must be an integer vector of length 2. Terms that appear in less
	documents than the lower bound \code{bounds$global[1]} or in
	more documents than the upper bound \code{bounds$global[2]} are
	discarded. Defaults to \code{list(global = c(1,Inf))} (i.e., every
	term will be used).}
      \item{\code{weighting}}{A weighting function capable of handling a
	\code{TermDocumentMatrix}. It defaults to \code{weightTf} for term
	frequency weighting. Available weighting functions shipped with
	the \pkg{tm} package are \code{\link{weightTf}},
	\code{\link{weightTfIdf}}, \code{\link{weightBin}}, and
	\code{\link{weightSMART}}.}
    }}
    \item{\dots}{the additional argument \code{weighting} (typically a
    \code{\link{WeightFunction}}) is allowed when coercing a
    simple triplet matrix to a term-document or document-term matrix.}
}
\value{
  An object of class \code{TermDocumentMatrix} or class
  \code{DocumentTermMatrix} (both inheriting from a
  \link[slam:matrix]{simple triplet matrix} in package \pkg{slam})
  containing a sparse term-document matrix or document-term matrix. The
  attribute \code{Weighting} contains the weighting applied to the
  matrix.
}
\seealso{
  \code{\link{termFreq}} for available local control options.
}
\examples{
data("crude")
tdm <- TermDocumentMatrix(crude,
                          control = list(removePunctuation = TRUE,
                                         stopwords = TRUE))
dtm <- DocumentTermMatrix(crude,
                          control = list(weighting =
                                         function(x)
                                         weightTfIdf(x, normalize =
                                                     FALSE),
                                         stopwords = TRUE))
inspect(tdm[155:160,1:5])
inspect(tdm[c("price", "texas"),c("127","144","191","194")])
inspect(dtm[1:5,155:160])
}
\author{Ingo Feinerer}
