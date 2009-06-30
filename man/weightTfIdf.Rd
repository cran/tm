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
  \item{m}{A \code{\link{TermDocumentMatrix}} in term frequency format.}
}
\details{
  Formally this function is a \code{WeightingFunction} with the additional
  slots \code{Name} and \code{Acronym}.
}
\value{
  The weighted matrix.
}
\author{Ingo Feinerer}
