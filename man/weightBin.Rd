\name{weightBin}
\alias{weightBin}
\title{Weight Binary}
\description{
  Binary weight a term-document matrix.
}
\usage{
weightBin(m)
}
\arguments{
  \item{m}{A \code{dgCMatrix} in term frequency format.}
}
\details{
  Formally this function is a \code{WeightingFunction} with the additional
  slots \code{Name} and \code{Acronym}.
}
\value{
  The weighted \code{dgCMatrix}.
}
\author{Ingo Feinerer}
\keyword{file}
