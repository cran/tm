\name{weightTf}
\alias{weightTf}
\title{Weight by Term Frequency}
\description{
  Weight a term-document matrix by term frequency.
}
\usage{
weightTf(m)
}
\arguments{
  \item{m}{A \code{\link{TermDocumentMatrix}} in term frequency format.}
}
\details{
  Formally this function is of class \code{WeightingFunction} with the
  additional attributes \code{Name} and \code{Acronym}.

  This function acts as the identity function since the input matrix is
  already in term frequency format.
}
\value{
  The weighted matrix.
}
\author{Ingo Feinerer}
