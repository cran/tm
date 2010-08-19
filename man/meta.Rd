\name{meta}
\alias{content_meta<-}
\alias{DublinCore}
\alias{DublinCore<-}
\alias{meta}
\alias{meta<-}
\alias{meta.TextDocument}
\alias{meta<-.TextDocument}
\alias{meta.Corpus}
\alias{meta<-.Corpus}
\alias{meta.TextRepository}
\alias{meta<-.TextRepository}
\title{Meta Data Management}
\description{
  Methods to access and modify meta data of documents, corpora, and
  repositories. In addition to \pkg{tm}'s internal meta data structures,
  Simple Dublin Core meta data mappings are available.
}
\usage{
\method{meta}{Corpus}(x, tag, type = c("indexed", "corpus", "local"))
\method{meta}{TextDocument}(x, tag, type = NULL)
\method{meta}{TextRepository}(x, tag, type = NULL)
content_meta(x, tag) <- value
DublinCore(x, tag = NULL)
}
\arguments{
  \item{x}{Either a text document, a corpus, or a text repository.}
  \item{tag}{A character identifying the name of the meta datum.}
  \item{type}{A character specifying which meta data of a corpus should
    be considered.}
  \item{value}{Replacement value.}
}
\details{
  In general this function can be used to display meta information but
  also to modify individual meta data:
  \describe{
    \item{x = "TextDocument", tag = NULL}{If no \code{tag} is given, this
      method pretty prints all \code{x}'s meta data. If \code{tag}
      is provided its value in the meta data is returned.}
    \item{x = "Corpus", tag = NULL, type = "indexed"}{This
      method investigates the \code{type} argument. \code{type} must be
      either \code{indexed} (default), \code{local}, or
      \code{corpus}. Former is a shortcut for accessing document level
      meta data (\code{\link{DMetaData}}) stored at the collection level
      (because it forms an own entity, or for performance reasons, i.e.,
      a form of indexing, hence the name indexed), \code{local} accesses
      the meta data local to each text document (i.e., meta data in text
      documents' attributes), and \code{corpus} is a shortcut for corpus
      specific meta data (\code{\link{CMetaData}}). Depending whether a
      tag is set or not, all or only the meta data identified by the tag
      is displayed or modified.}
    \item{x = "TextRepository", tag = NULL}{If no \code{tag} is given, this
      method pretty prints all \code{x}'s meta data. If \code{tag}
      is provided its value in the meta data is returned.}
  }

  Simple Dublin Core meta data is only available locally at each
  document:
  \describe{
    \item{x = "TextDocument", tag = NULL}{Returns or sets the
      Simple Dublin Core meta datum named \code{tag} for
      \code{x}. \code{tag} must be a valid Simple Dublin Core
      element name (i.e, title, creator, subject, description,
      publisher, contributor, date, type, format, identifier, source,
      language, relation, coverage, or rights) or \code{NULL}. For the
      latter all Dublin Core meta data are printed.}
  }

  \code{content_meta} is a convenience wrapper which calls
  \code{Content} if \code{tag = "Content"} and \code{meta} otherwise. 
}
\references{
  Dublin Core Metadata Initiative.
  \url{http://dublincore.org/}
}
\examples{
data("crude")
meta(crude[[1]])
DublinCore(crude[[1]])
meta(crude[[1]], tag = "Topics")
meta(crude[[1]], tag = "Comment") <- "A short comment."
meta(crude[[1]], tag = "Topics") <- NULL
DublinCore(crude[[1]], tag = "creator") <- "Ano Nymous"
DublinCore(crude[[1]], tag = "Format") <- "XML"
DublinCore(crude[[1]])
meta(crude[[1]])
meta(crude)
meta(crude, type = "corpus")
meta(crude, "labels") <- 21:40
meta(crude)
}
