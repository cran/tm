\name{tm_combine}
\alias{c.Corpus}
\alias{c.TextDocument}
\title{Combine Corpora and Documents}
\description{
  Combine serveral corpora into a single one or combine multiple
  documents into a corpus.
}
\usage{
\method{c}{Corpus}(x, \dots, recursive = FALSE)
\method{c}{TextDocument}(x, \dots, recursive = FALSE)
}
\arguments{
  \item{x}{A corpus or a text document.}
  \item{\dots}{Corpora or text documents.}
  \item{recursive}{Logical. Provided by generic function definition but
    not used.}
}
\details{
  Meta data from input objects (corpora or documents) is preserved
  during concatenation and intelligently merged into the newly created
  corpus. Although we use a sophisticated merging strategy (by using a
  binary tree for corpus specific meta data and by joining document
  level specific meta data in data frames) you should check the newly
  created meta data for consistency when merging corpora with (partly)
  identical meta data. However, in most cases the meta data merging
  strategy will produce validly combined and arranged meta data
  structures.
}
\examples{
data("acq")
data("crude")
summary(c(acq,crude))
summary(c(acq[[30]],crude[[10]]))
}
