\name{VCorpus}
\alias{VCorpus}
\alias{Corpus}
\alias{CMetaData}
\alias{CMetaData.Corpus}
\alias{DMetaData}
\alias{DMetaData.VCorpus}
\alias{DMetaData<-}
\alias{DMetaData<-.VCorpus}
\title{Volatile Corpus}
\description{
  Data structures and operators for volatile corpora.
}
\usage{
Corpus(x, readerControl = list(reader = x$DefaultReader, language = "en"))
VCorpus(x, readerControl = list(reader = x$DefaultReader, language = "en"))
\method{DMetaData}{VCorpus}(x)
\method{CMetaData}{Corpus}(x)
}
\arguments{
  \item{x}{A \code{\link{Source}} object for \code{Corpus} and
    \code{VCorpus}, and a corpus for the other functions.}
  \item{readerControl}{A list with the named components \code{reader}
    representing a reading function capable of handling the file format
    found in \code{x}, and \code{language} giving the text's language
    (preferably as \acronym{IETF} language tags). The default language
    is assumed to be English (\code{"en"}). Use \code{NA} to avoid
    internal assumptions (e.g., when the language is unknown or is
    deliberately not set).}
}
\value{
  An object of class \code{VCorpus} which extends the classes
  \code{Corpus} and \code{list} containing a collection of text
  documents.
}
\details{
  Volatile means that the corpus is fully kept in memory and thus all
  changes only affect the corresponding \R object. In contrast there is
  also a corpus implementation available providing a permanent semantics
  (see \code{\link{PCorpus}}).

  The constructed corpus object inherits from a \code{list} and has two
  attributes containing meta information:
  \describe{
    \item{\code{CMetaData}}{Corpus Meta Data contains corpus specific
      meta data in form of tag-value pairs and information about
      children in form of a binary tree. This information is useful for
      reconstructing meta data after e.g. merging corpora.}
    \item{\code{DMetaData}}{Document Meta Data of class
      \code{data.frame} contains document specific meta data for the
      corpus. This data frame typically encompasses clustering or
      classification results which basically are metadata for documents
      but form an own entity (e.g., with its name, the value range,
      etc.).}
  }
}
\examples{
reut21578 <- system.file("texts", "crude", package = "tm")
(r <- Corpus(DirSource(reut21578),
             readerControl = list(reader = readReut21578XMLasPlain)))
}
\author{Ingo Feinerer}
