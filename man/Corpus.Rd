\name{Corpus}
\alias{Corpus}
\title{Corpora}
\description{
  Representing and computing on corpora.
}
\details{
  \emph{Corpora} are collections of documents containing (natural language)
  text. In packages which employ the infrastructure provided by package
  \pkg{tm}, such corpora are represented via the virtual S3 class
  \code{Corpus}: such packages then provide S3 corpus classes extending the
  virtual base class (such as \code{\link{VCorpus}} provided by package \pkg{tm}
  itself).

  All extension classes must provide accessors to extract subsets
  (\code{\link{[}}), individual documents (\code{\link{[[}}), and metadata
  (\code{\link{meta}}). The function \code{\link{length}} must return the number
  of documents, and \code{\link{as.list}} must construct a list holding the
  documents.

  A corpus can have two types of metadata (accessible via \code{\link{meta}}).
  \emph{Corpus metadata} contains corpus specific metadata in form of tag-value
  pairs. \emph{Document level metadata} contains document specific metadata but
  is stored in the corpus as a data frame. Document level metadata is typically
  used for semantic reasons (e.g., classifications of documents form an own
  entity due to some high-level information like the range of possible values)
  or for performance reasons (single access instead of extracting metadata of
  each document).

  The function \code{Corpus} is a convenience alias to \code{SimpleCorpus} or
  \code{VCorpus}, depending on the arguments provided.
}
\seealso{
  \code{\link{SimpleCorpus}}, \code{\link{VCorpus}}, and \code{\link{PCorpus}}
  for the corpora classes provided by package \pkg{tm}.

  \code{\link[tm.plugin.dc]{DCorpus}} for a distributed corpus class provided by
  package \pkg{tm.plugin.dc}.
}
