\name{Dictionary}
\alias{Dictionary}
\alias{Dictionary.character}
\alias{Dictionary.TermDocumentMatrix}
\title{Dictionary}
\description{
  Constructs a dictionary from a character vector or a term-document matrix.
}
\usage{
\method{Dictionary}{character}(x)
\method{Dictionary}{TermDocumentMatrix}(x)
}
\arguments{
  \item{x}{A character vector or a term-document matrix holding the terms for the dictionary.}
}
\value{
  An S4 object of class \code{\linkS4class{Dictionary}} which extends the class
  \code{character} representing a dictionary.
}
\examples{
Dictionary(c("some", "tokens"))
data(crude)
Dictionary(TermDocumentMatrix(crude))
}
\author{Ingo Feinerer}
