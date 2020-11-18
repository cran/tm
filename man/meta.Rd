\name{meta}
\alias{DublinCore}
\alias{DublinCore<-}
\alias{meta}
\alias{meta.PCorpus}
\alias{meta.SimpleCorpus}
\alias{meta.VCorpus}
\alias{meta<-.PCorpus}
\alias{meta<-.SimpleCorpus}
\alias{meta<-.VCorpus}
\alias{meta.PlainTextDocument}
\alias{meta<-.PlainTextDocument}
\alias{meta.XMLTextDocument}
\alias{meta<-.XMLTextDocument}
\title{Metadata Management}
\description{
  Accessing and modifying metadata of text documents and corpora.
}
\usage{
\method{meta}{PCorpus}(x, tag = NULL, type = c("indexed", "corpus", "local"), \dots)
\method{meta}{PCorpus}(x, tag, type = c("indexed", "corpus", "local"), \dots) <- value
\method{meta}{SimpleCorpus}(x, tag = NULL, type = c("indexed", "corpus"), \dots)
\method{meta}{SimpleCorpus}(x, tag, type = c("indexed", "corpus"), \dots) <- value
\method{meta}{VCorpus}(x, tag = NULL, type = c("indexed", "corpus", "local"), \dots)
\method{meta}{VCorpus}(x, tag, type = c("indexed", "corpus", "local"), \dots) <- value
\method{meta}{PlainTextDocument}(x, tag = NULL, \dots)
\method{meta}{PlainTextDocument}(x, tag = NULL, \dots) <- value
\method{meta}{XMLTextDocument}(x, tag = NULL, \dots)
\method{meta}{XMLTextDocument}(x, tag = NULL, \dots) <- value
DublinCore(x, tag = NULL)
DublinCore(x, tag) <- value
}
\arguments{
  \item{x}{For \code{DublinCore} a \code{\link{TextDocument}}, and for
    \code{meta} a \code{\link{TextDocument}} or a \code{\link{Corpus}}.}
  \item{tag}{a character giving the name of a metadatum. No tag corresponds to
    all available metadata.}
  \item{type}{a character specifying the kind of corpus metadata (see
    \bold{Details}).}
  \item{\dots}{Not used.}
  \item{value}{replacement value.}
}
\details{
  A corpus has two types of metadata. \emph{Corpus metadata} (\code{"corpus"})
  contains corpus specific metadata in form of tag-value pairs.
  \emph{Document level metadata} (\code{"indexed"}) contains document specific
  metadata but is stored in the corpus as a data frame. Document level metadata
  is typically used for semantic reasons (e.g., classifications of documents
  form an own entity due to some high-level information like the range of
  possible values) or for performance reasons (single access instead of
  extracting metadata of each document). The latter can be seen as a from of
  indexing, hence the name \code{"indexed"}. \emph{Document metadata}
  (\code{"local"}) are tag-value pairs directly stored locally at the individual
  documents.

  \code{DublinCore} is a convenience wrapper to access and modify the metadata
  of a text document using the Simple Dublin Core schema (supporting the 15
  metadata elements from the Dublin Core Metadata Element Set
  \url{https://dublincore.org/documents/dces/}).
}
\seealso{
  \code{\link[NLP]{meta}} for metadata in package \pkg{NLP}.
}
\references{
  Dublin Core Metadata Initiative.
  \url{https://dublincore.org/}
}
\examples{
data("crude")
meta(crude[[1]])
DublinCore(crude[[1]])
meta(crude[[1]], tag = "topics")
meta(crude[[1]], tag = "comment") <- "A short comment."
meta(crude[[1]], tag = "topics") <- NULL
DublinCore(crude[[1]], tag = "creator") <- "Ano Nymous"
DublinCore(crude[[1]], tag = "format") <- "XML"
DublinCore(crude[[1]])
meta(crude[[1]])
meta(crude)
meta(crude, type = "corpus")
meta(crude, "labels") <- 21:40
meta(crude)
}
