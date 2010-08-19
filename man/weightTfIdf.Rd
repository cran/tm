\name{weightTfIdf}
\alias{weightTfIdf}
\title{Weight by Term Frequency - Inverse Document Frequency}
\description{
  Weight a term-document matrix by term frequency - inverse document
  frequency.
}
\usage{
weightTfIdf(m, normalize = TRUE)
}
\arguments{
  \item{m}{A \code{\link{TermDocumentMatrix}} in term frequency format.}
  \item{normalize}{A Boolean value indicating whether the term
    frequencies should be normalized.}
}
\details{
  Formally this function is of class \code{WeightingFunction} with the
  additional attributes \code{Name} and \code{Acronym}.

  \emph{Term frequency} \eqn{\mathit{tf}_{i,j}} counts the number of
  occurrences \eqn{n_{i,j}} of a term \eqn{t_i} in a document
  \eqn{d_j}. In the case of normalization, the term frequency
  \eqn{\mathit{tf}_{i,j}} is divided by \eqn{\sum_k n_{k,j}}.

  \emph{Inverse document frequency} for a term \eqn{t_i} is defined as
  \deqn{\mathit{idf}_i = \log \frac{|D|}{|\{d \mid t_i \in d\}|}} where
  \eqn{|D|} denotes the total number of documents and where \eqn{|\{d
  \mid t_i \in d\}|} is the number of documents where the term \eqn{t_i}
  appears.

  \emph{Term frequency - inverse document frequency} is now defined as
  \eqn{\mathit{tf}_{i,j} \cdot \mathit{idf}_i}.
}
\value{
  The weighted matrix.
}
\references{
  Gerard Salton and Christopher Buckley (1988).
  Term-weighting approaches in automatic text retrieval.
  \emph{Information Processing and Management}, \bold{24}/5, 513--523.
}
\author{Ingo Feinerer}
