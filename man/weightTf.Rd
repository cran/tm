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
  \item{m}{a \code{dgCMatrix} in term frequency format.}
}
\details{
  Formally this function is a \code{WeightingFunction} with the additional
  slot \code{Name}.

  This function acts as the identity function since the input matrix is
  already in term frequency format.
}
\value{
  The weighted \code{dgCMatrix}.
}
\author{Ingo Feinerer}
\keyword{file}
