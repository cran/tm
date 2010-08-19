\name{TextRepository}
\alias{TextRepository}
\alias{RepoMetaData}
\title{Text Repository}
\description{
  Construct a text repository for corpora. A repository is designed to
  keep track of multiple corpora, either different ones, or corpora with
  the same underlying texts but at different preprocessing stages.
}
\usage{
TextRepository(x,
               meta = list(created =
                           as.POSIXlt(Sys.time(), tz = "GMT")))
}
\arguments{
  \item{x}{A corpus.}
  \item{meta}{An initial list of tag-value pairs for the repository meta data.}
}
\value{
  An object of class \code{TextRepository} which extends the class
  \code{list} containing corpora. Meta data annotations are stored in
  the attribute \code{RepoMetaData} in form of tag-value pairs (i.e., a
  named list).
}
\examples{
data("crude")
repo <- TextRepository(crude)
summary(repo)
RepoMetaData(repo)
}
\author{Ingo Feinerer}
