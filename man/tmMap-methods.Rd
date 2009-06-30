\name{tmMap-methods}
\docType{methods}
\alias{tmMap}
\alias{tmMap-methods}
\alias{tmMap,VCorpus,function-method}
\alias{tmMap,PCorpus,function-method}
\title{Methods for Function tmMap in Package `tm'}
\description{
Methods for function \code{tmMap} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "Corpus", FUN, ..., lazy = FALSE}{Applies \code{FUN} onto each element
      of \code{object}. Note that document specific metadata (i.e.,
      \code{DMetaData(object)} is automatically passed to
      \code{FUN} as argument \code{DMetaData}). \code{FUN} must be a
      function which returns a \code{TextDocument}. If \code{lazy} is
      set, so-called lazy mapping is activated, i.e., mappings are
      delayed until the documents' content is accessed. Lazy mapping is
      useful when working with large corpora but only a few documents
      are accessed, as it avoids applying the mapping to all elements in
      the corpus.}
  }
}
\seealso{
  See \code{\link{getTransformations}} for available transformations
  shipped with \pkg{tm}. See \code{\link{materialize}} for manually
  materializing lazy transformations.
}
\note{
Please be aware that lazy transformations are an experimental feature and
change \R's standard evaluation semantics.
}
\examples{
data("crude")
tmMap(crude, stemDoc)

headings <- function(object, ...) {
new("PlainTextDocument", Heading(object), DateTimeStamp =
Sys.time(), ID = ID(object), Language = Language(object))
}
inspect(tmMap(crude, headings))
}
\keyword{methods}
