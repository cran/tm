\name{TextRepository}
\docType{methods}
\alias{TextRepository}
\alias{TextRepository,TextDocCol-method}
\title{Text document collection}
\description{
  Constructs a text document collection from various sources.
}
\usage{
\S4method{TextRepository}{TextDocCol}(object, metaname = "created", meta = date())
}
\arguments{
  \item{object}{a directory containing the documents.}
  \item{metaname}{an initial tag for the representational metadata}
  \item{meta}{an initial value for the \code{metaname} tag}
}
\value{
  An S4 object of class \code{TextRepository} which extends the class
  \code{list} containing text document collections.
}
\author{Ingo Feinerer}
\keyword{methods}
