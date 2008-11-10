\name{removeWords-methods}
\docType{methods}
\alias{removeWords}
\alias{removeWords-methods}
\alias{removeWords,PlainTextDocument,character-method}
\title{Methods for Function removeWords in Package `tm'}
\description{
Methods for function \code{removeWords} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "PlainTextDocument", words = "character"}{Removes all
      words which occur in \code{words} from \code{object} and return the object.}
  }
}
\seealso{
  Use \code{\link{getTransformations}} to list available transformation (mapping) functions.
}
\examples{
data("crude")
crude[[1]]
removeWords(crude[[1]], stopwords("english"))
}
\keyword{methods}
