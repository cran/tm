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
  \item{m}{A \code{\link{TermDocumentMatrix}} in term frequency format.}
}
\details{
  Formally this function is of class \code{WeightingFunction} with the
  additional attributes \code{Name} and \code{Acronym}.
}
\value{
  The weighted matrix.
}
\author{Ingo Feinerer}
