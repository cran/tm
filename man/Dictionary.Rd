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
  An object of class \code{Dictionary} which extends the class
  \code{character} representing a dictionary, i.e., a character vector
  of terms.
}
\examples{
Dictionary(c("some", "tokens"))
data(crude)
Dictionary(TermDocumentMatrix(crude))
}
\author{Ingo Feinerer}
