\name{weightTfIdf}
\alias{weightTfIdf}
\title{Weight By Term Frequency Inverse Document Frequency}
\description{
  Weight a term-document matrix by term frequency - inverse document frequency.
}
\usage{
weightTfIdf(m)
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
