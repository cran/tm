\name{TextRepository}
\docType{methods}
\alias{TextRepository}
\alias{TextRepository,Corpus-method}
\title{Text Repository}
\description{
  Constructs a text repository.
}
\usage{
\S4method{TextRepository}{Corpus}(object, meta = list(created = Sys.time()))
}
\arguments{
  \item{object}{A directory containing the documents.}
  \item{meta}{An initial list of tag-value pairs for the meta data.}
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
