\name{number}
\alias{ncol.TermDocumentMatrix}
\alias{dim.TermDocumentMatrix}
\alias{nrow.TermDocumentMatrix}
\alias{ncol.DocumentTermMatrix}
\alias{dim.DocumentTermMatrix}
\alias{nrow.DocumentTermMatrix}
\alias{nDocs}
\alias{nTerms}
\title{The Number of Rows/Columns/Dimensions/Documents/Terms of a Term-Document Matrix}
\description{
  Return the number of rows, columns, dimensions, documents, and terms
  of a term-document matrix or a document-term matrix.
}
\arguments{
  \item{x}{Either a term-document matrix or a document-term matrix.}
}
\examples{
data("crude")
tdm <- TermDocumentMatrix(crude)[1:10,1:20]
ncol(tdm)
nrow(tdm)
dim(tdm)
nDocs(tdm)
nTerms(tdm)
}
