\name{makeChunks}
\alias{makeChunks}
\title{Split a Corpus into Chunks}
\description{
  Split a corpus into equally sized chunks conserving document
  boundaries.
}
\usage{
makeChunks(corpus, chunksize)
}
\arguments{
  \item{corpus}{The corpus to be split into chunks.}
  \item{chunksize}{The chunk size.}
}
\value{
  A corpus consisting of the chunks. Note that corpus meta data is not
  passed on to the newly created chunk corpus.
}
\author{Ingo Feinerer}
\examples{
txt <- system.file("texts", "txt", package = "tm")
ovid <- Corpus(DirSource(txt))
sapply(ovid, length)
ovidChunks <- makeChunks(ovid, 5)
sapply(ovidChunks, length)
}
