\name{VCorpus}
\docType{methods}
\alias{VCorpus}
\alias{Corpus}
\alias{VCorpus,Source-method}
\alias{coerce,list,VCorpus-method}
\title{Volatile Corpus Constructor}
\description{
  Constructs a volatile corpus.
}
\usage{
Corpus(object, readerControl = list(reader = object@DefaultReader, language = "eng"), \dots)
}
\arguments{
  \item{object}{A \code{Source} object.}
  \item{readerControl}{A list with the named components \code{reader}
    representing a reading function capable of handling the file format
    found in \code{object}, and \code{language} giving the text's language
    (preferably in \acronym{ISO} 639-2 format).}
  \item{...}{Optional arguments for the \code{reader}.}
}
\value{
  An S4 object of class \code{VCorpus} which extends the class
  \code{list} containing a corpus.
}
\examples{
reut21578 <- system.file("texts", "reut21578", package = "tm")
Corpus(DirSource(reut21578), readerControl = list(reader = readReut21578XML))
}
\author{Ingo Feinerer}
