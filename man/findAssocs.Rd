\name{findAssocs}
\alias{findAssocs}
\alias{findAssocs.DocumentTermMatrix}
\alias{findAssocs.TermDocumentMatrix}
\title{Find Associations in a Term-Document Matrix}
\description{
  Find associations in a document-term or term-document matrix.
}
\usage{
\method{findAssocs}{DocumentTermMatrix}(x, terms, corlimit)
\method{findAssocs}{TermDocumentMatrix}(x, terms, corlimit)
}
\arguments{
  \item{x}{A \code{\link{DocumentTermMatrix}} or a
    \code{\link{TermDocumentMatrix}}.}
  \item{terms}{a character vector holding terms.}
  \item{corlimit}{a numeric vector (of the same length as \code{terms};
    recycled otherwise) for the (inclusive) lower correlation limits of each
    term in the range from zero to one.}
}
\value{A named list. Each list component is named after a term in \code{terms}
and contains a named numeric vector. Each vector holds matching terms from
\code{x} and their rounded correlations satisfying the inclusive lower
correlation limit of \code{corlimit}.}
\examples{
data("crude")
tdm <- TermDocumentMatrix(crude)
findAssocs(tdm, c("oil", "opec", "xyz"), c(0.7, 0.75, 0.1))
}
