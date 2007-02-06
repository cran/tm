\name{TextRepository}
\docType{methods}
\alias{TextRepository}
\alias{TextRepository,Corpus-method}
\title{Text repository}
\description{
  Constructs a text document collection from various sources.
}
\usage{
\S4method{TextRepository}{Corpus}(object, meta = list(created = Sys.time()))
}
\arguments{
  \item{object}{a directory containing the documents.}
  \item{meta}{an initial list of tag-value pairs for the representational metadata}
}
\value{
  An S4 object of class \code{TextRepository} which extends the class
  \code{list} containing text document collections.
}
\examples{
data("crude")
repo <- TextRepository(crude)
summary(repo)
RepoMetaData(repo)
}
\author{Ingo Feinerer}
\keyword{methods}
