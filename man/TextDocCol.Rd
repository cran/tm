\name{Corpus}
\docType{methods}
\alias{Corpus}
\alias{Corpus,Source-method}
\alias{coerce,list,Corpus-method}
\title{Corpus}
\description{
  Constructs a corpus (= text document collection).
}
\usage{
\S4method{Corpus}{Source}(object, readerControl = list(reader = object@DefaultReader,
language = "en_US", load = TRUE), dbControl = list(useDb = FALSE, dbName = "",
dbType = "DB1"), ...)
}
\arguments{
  \item{object}{A \code{Source} object.}
  \item{readerControl}{A list with the named components \code{reader}
    representing a reading function capable of handling the file format
    found in \code{object}, \code{language} giving the text's language
    (preferably in \acronym{Iso} 639-1 format), and
    \code{load} being a logical value indicating whether the text corpus of
    documents should be loaded immediately into memory (\code{load = TRUE}) or loaded when
    necessary (\code{load = FALSE}). This allows to minimize memory
    demands for large document collections. If \code{object} does not
    support load on demand the text corpus is automatically loaded,
    i.e., this argument is overruled.}
  \item{dbControl}{A list with the named components \code{useDb}
    indicating that database support should be activated, \code{dbName}
    giving the filename holding the sourced out objects (i.e., the
    database), and \code{dbType} holding a valid database type as
    supported by package \pkg{filehash}. Under activated database
    support the \code{tm} package tries to keep as few as possible
    resources in memory under usage of the database.}
  \item{...}{Optional arguments for the \code{reader}.}
}
\value{
  An S4 object of class \code{Corpus} which extends the class
  \code{list} containing a collection of text documents.
}
\examples{
txt <- system.file("texts", "txt", package = "tm")
\dontrun{(Corpus(DirSource(txt), readerControl = list(reader
= readPlain, language = "en_US", load = TRUE), dbControl = list(useDb =
TRUE, dbName = "oviddb", dbType = "DB1")))}
reut21578 <- system.file("texts", "reut21578", package = "tm")
Corpus(DirSource(reut21578), readerControl = list(reader = readReut21578XML, language = "en_US", load = FALSE))
}
\author{Ingo Feinerer}
\keyword{methods}
