\name{readDataframe}
\alias{readDataframe}
\title{Read In a Text Document from a Data Frame}
\description{
  Read in a text document from a row in a data frame.
}
\usage{
readDataframe(elem, language, id)
}
\arguments{
  \item{elem}{a named list with the component \code{content} which must hold
    a data frame with rows as the documents to be read in. The names of the
    columns holding the text content and the document identifier must be
    \code{"text"} and \code{"doc_id"}, respectively.}
  \item{language}{a string giving the language.}
  \item{id}{Not used.}
}
\value{
  A \code{\link{PlainTextDocument}} representing \code{elem$content}.
}
\seealso{
  \code{\link{Reader}} for basic information on the reader infrastructure
  employed by package \pkg{tm}.
}
\examples{
docs <- data.frame(doc_id = c("doc_1", "doc_2"),
                   text = c("This is a text.", "This another one."),
                   stringsAsFactors = FALSE)
ds <- DataframeSource(docs)
elem <- getElem(stepNext(ds))
result <- readDataframe(elem, "en", NULL)
inspect(result)
meta(result)
}
