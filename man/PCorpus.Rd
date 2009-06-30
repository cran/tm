\name{PCorpus}
\docType{methods}
\alias{PCorpus}
\alias{PCorpus,Source-method}
\title{Permanent Corpus Constructor}
\description{
  Constructs a permanent corpus.
}
\usage{
PCorpus(object, readerControl = list(reader = object@DefaultReader, language = "eng"), dbControl = list(dbName = "", dbType = "DB1"), \dots)
}
\arguments{
  \item{object}{A \code{Source} object.}
  \item{readerControl}{A list with the named components \code{reader}
    representing a reading function capable of handling the file format
    found in \code{object}, and \code{language} giving the text's language
    (preferably in \acronym{ISO} 639-2 format).}
  \item{dbControl}{A list with the named components \code{dbName}
    giving the filename holding the sourced out objects (i.e., the
    database), and \code{dbType} holding a valid database type as
    supported by package \pkg{filehash}. Under activated database
    support the \code{tm} package tries to keep as few as possible
    resources in memory under usage of the database.}
  \item{...}{Optional arguments for the \code{reader}.}
}
\value{
  An S4 object of class \code{PCorpus} which extends the class
  \code{list} containing a permanent corpus.
}
\examples{
txt <- system.file("texts", "txt", package = "tm")
\dontrun{PCorpus(DirSource(txt), dbControl = list(dbName = "myDB.db", dbType = "DB1"))}
}
\author{Ingo Feinerer}
