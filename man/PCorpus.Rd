\name{PCorpus}
\docType{methods}
\alias{PCorpus}
\alias{DBControl}
\alias{DMetaData.PCorpus}
\alias{DMetaData<-.PCorpus}
\title{Permanent Corpus Constructor}
\description{
  Construct a permanent corpus.
}
\usage{
PCorpus(x,
        readerControl = list(reader = x$DefaultReader, language = "en"),
        dbControl = list(dbName = "", dbType = "DB1"))
DBControl(x)
\method{DMetaData}{PCorpus}(x)
}
\arguments{
  \item{x}{A \code{\link{Source}} object for \code{PCorpus}, and a
    corpus for the other functions.}
  \item{readerControl}{A list with the named components \code{reader}
    representing a reading function capable of handling the file format
    found in \code{x}, and \code{language} giving the text's language
    (preferably as \acronym{IETF} language tags). The default language
    is assumed to be English (\code{"en"}). Use \code{NA} to avoid
    internal assumptions (e.g., when the language is unknown or is
    deliberately not set).}
  \item{dbControl}{A list with the named components \code{dbName}
    giving the filename holding the sourced out documents (i.e., the
    database), and \code{dbType} holding a valid database type as
    supported by package \pkg{filehash}. Under activated database
    support the \code{tm} package tries to keep as few as possible
    resources in memory under usage of the database.}
}
\value{
  An object of class \code{PCorpus} which extends the classes
  \code{Corpus} and \code{list} containing a permanent corpus.
}
\details{
  Permanent means that documents are physically stored outside of \R
  (e.g., in a database) and \R objects are only pointers to external
  structures. I.e., changes in the underlying external representation
  can affect multiple \R objects simultaneously.
  
  The constructed corpus object inherits from a \code{list} and has
  three attributes containing meta and database management
  information:
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
    \item{\code{DBControl}}{Database control field is a \code{list} with
      two named components: \code{dbName} holds the path to the
      permanent database storage, and \code{dbType} stores the database
      type.}
  }
}
\examples{
txt <- system.file("texts", "txt", package = "tm")
\dontrun{PCorpus(DirSource(txt),
        dbControl = list(dbName = "myDB.db", dbType = "DB1"))}
}
\author{Ingo Feinerer}
