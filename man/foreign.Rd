\name{foreign}
\alias{read_dtm_Blei_et_al}
\alias{read_dtm_MC}
\title{Read Document-Term Matrices}
\description{
  Read document-term matrices stored in special file formats.
}
\usage{
read_dtm_Blei_et_al(file, vocab = NULL)
read_dtm_MC(file, scalingtype = NULL)
}
\arguments{
  \item{file}{a character string with the name of the file to read.}
  \item{vocab}{a character string with the name of a vocabulary file
    (giving the terms, one per line), or \code{NULL}.}
  \item{scalingtype}{a character string specifying the type of scaling
    to be used, or \code{NULL} (default), in which case the scaling will
    be inferred from the names of the files with non-zero entries found
    (see \bold{Details}).}
}
\details{
  \code{read_dtm_Blei_et_al} reads the (List of Lists type sparse
  matrix) format employed by the Latent Dirichlet Allocation and
  Correlated Topic Model C codes by Blei et al
  (\url{http://www.cs.princeton.edu/~blei}).

  MC is a toolkit for creating vector models from text documents (see 
  \url{http://www.cs.utexas.edu/users/dml/software/mc/}).  It employs a
  variant of Compressed Column Storage (CCS) sparse matrix format,
  writing data into several files with suitable names: e.g., a file with
  \file{_dim} appended to the base file name stores the matrix
  dimensions.  The non-zero entries are stored in a file the name of
  which indicates the scaling type used: e.g., \file{_tfx_nz} indicates
  scaling by term frequency (\samp{t}), inverse document frequency
  (\samp{f}) and no normalization (\samp{x}).  See \file{README} in the
  MC sources for more information.

  \code{read_dtm_MC} reads such sparse matrix information with argument
  \code{file} giving the path with the base file name.
}
\value{
  A \link[=DocumentTermMatrix]{document-term matrix}.
}
\seealso{
  \code{\link[slam]{read_stm_MC}} in package \pkg{slam}.
}
\keyword{IO}
