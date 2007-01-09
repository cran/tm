\name{TextDocCol}
\docType{methods}
\alias{TextDocCol}
\alias{TextDocCol,Source-method}
\title{Text document collection}
\description{
  Constructs a text document collection.
}
\usage{
\S4method{TextDocCol}{Source}(object, parser = read_plain, load = FALSE,...)
}
\arguments{
  \item{object}{a \code{Source} object.}
  \item{parser}{a parsing function capable of handling the file format
    found in \code{object}.}
  \item{load}{a logical value indicating whether the text corpus of
    documents should
    be loaded immediately into memory (\code{load = TRUE}) or loaded when
    necessary (\code{load = FALSE}). This allows to minimize memory
    demands for large document collections. If \code{object} does not
    support load on demand the text corpus is automatically loaded,
    i.e., this argument is overruled.}
  \item{...}{optional arguments for the \code{parser}.}
}
\value{
  An S4 object of class \code{TextDocCol} which extends the class
  \code{list} containing a collection of text documents.
}
\author{Ingo Feinerer}
\keyword{methods}
