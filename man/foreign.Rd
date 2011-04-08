\name{foreign}
\alias{read_dtm_Blei_et_al}
\title{Read Document-Term Matrices}
\description{
  Read document-term matrices stored in special file formats.
}
\usage{
read_dtm_Blei_et_al(file, vocab = NULL)
}
\arguments{
  \item{file}{a character string with the name of the file to read.}
  \item{vocab}{a character string with the name of a vocabulary file
    (giving the terms, one per line), or \code{NULL}.}
}
\details{
  \code{read_dtm_Blei_et_al} reads the (List of Lists type sparse
  matrix) format employed by the Latent Dirichlet Allocation and
  Correlated Topic Model C codes by Blei et al
  (\url{http://www.cs.princeton.edu/~blei}).
}
\value{
  A document-term matrix.
}
\keyword{IO}
