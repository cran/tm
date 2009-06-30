\name{weightTf}
\alias{weightTf}
\title{Weight By Term Frequency}
\description{
  Weight a term-document matrix by term frequency.
}
\usage{
weightTf(x)
}
\arguments{
  \item{x}{A \code{\link{TermDocumentMatrix}} in term frequency format.}
}
\details{
  Formally this function is a \code{WeightingFunction} with the additional
  slots \code{Name} and \code{Acronym}.

  This function acts as the identity function since the input matrix is
  already in term frequency format.
}
\value{
  The weighted matrix.
}
\author{Ingo Feinerer}
