\name{weightTf}
\alias{weightTf}
\title{Weight By Term Frequency}
\description{
  Weight a term-document matrix by term frequency.
}
\usage{
weightTf(m)
}
\arguments{
  \item{m}{A \code{dgCMatrix} in term frequency format.}
}
\details{
  Formally this function is a \code{WeightingFunction} with the additional
  slots \code{Name} and \code{Acronym}.

  This function acts as the identity function since the input matrix is
  already in term frequency format.
}
\value{
  The weighted \code{dgCMatrix}.
}
\author{Ingo Feinerer}
\keyword{file}
