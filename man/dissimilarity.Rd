\name{dissimilarity}
\alias{dissimilarity}
\alias{dissimilarity.TermDocumentMatrix}
\alias{dissimilarity.PlainTextDocument}
\title{Dissimilarity}
\description{
  Compute the dissimilarity between documents in a term-document matrix
  or between two explicit documents.
}
\usage{
\method{dissimilarity}{TermDocumentMatrix}(x, y = NULL, method)
\method{dissimilarity}{PlainTextDocument}(x, y = NULL, method)
}
\arguments{
  \item{x}{Either a term-document matrix or a text document.}
  \item{y}{A text document. Only used if \code{x} is a text document.}
  \item{method}{Dissimilarity measure. Any method accepted by \code{dist}
    from package \pkg{proxy} can be passed over.}
}
\value{
  An object of class \code{dist} representing the dissimilarity between
  participating documents.
}
\examples{
data("crude")
tdm <- TermDocumentMatrix(crude)
dissimilarity(tdm, method = "cosine")
dissimilarity(crude[[1]], crude[[2]], method = "eJaccard")
}
