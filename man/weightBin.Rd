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
  \item{m}{a \code{dgCMatrix} in term frequency format.}
}
\details{
  Formally this function is a \code{WeightingFunction} with the additional
  slot \code{Name}.
}
\value{
  The weighted \code{dgCMatrix}.
}
\author{Ingo Feinerer}
\keyword{file}
