\name{TermDocMatrix}
\docType{methods}
\alias{TermDocMatrix}
\alias{TermDocMatrix,TextDocCol-method}
\title{Term-document matrix}
\description{
  Constructs a term-document matrix.
}
\usage{
\S4method{TermDocMatrix}{TextDocCol}(object, weighting = "tf", stemming
= FALSE, minWordLength = 3, minDocFreq = 1, stopwords = NULL)
}
\arguments{
  \item{object}{a text document collection}
  \item{weighting}{the weighting mode for the term-document
    matrix. Possible settings are
    \itemize{
      \item \code{tf} Term frequency
      \item \code{tf-idf} Term frequency inverse document frequency
      \item \code{bin} Binary frequency
      \item \code{logical} Similar to binary frequency but with Boolean values
    }
  }
  \item{stemming}{if set, stems words before making the term-document matrix.}
  \item{minWordLength}{words smaller than this number are discarded for
    the term-document matrix.}
  \item{minDocFreq}{words that appear less often in documents than this
    number are discarded for the term-document matrix.}
  \item{stopwords}{either a plain text file with all stopwords or a
    Boolean value. In the latter case the default stopwords in
    accordance with the documents' language are used.}
}
\value{
  An S4 object of class \code{TermDocMatrix} which extends the class
  \code{matrix} containing a term-document matrix. The following slots
  contain useful information:

  \item{Weighting}{The weighting mode applied to the term-document matrix}
}
\examples{
data("crude")
TermDocMatrix(crude, weighting = "tf-idf", stopwords = TRUE)
}
\author{Ingo Feinerer}
\keyword{methods}
