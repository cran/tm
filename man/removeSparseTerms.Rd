\name{removeSparseTerms-methods}
\docType{methods}
\alias{removeSparseTerms}
\alias{removeSparseTerms-methods}
\alias{removeSparseTerms,TermDocMatrix,numeric-method}
\title{Methods for Function removeSparseTerms in Package `tm'}
\description{
Methods for function \code{removeSparseTerms} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "TermDocMatrix", sparse = "numeric"}{Return
      \code{object} where those columns (i.e., terms) from
      \code{Data(object)} are removed which have at least a \code{sparse}
      percentage of sparse (i.e., terms occurring 0 times in a document)
      elements. I.e., the resulting matrix contains only columns with a
      sparse factor of less than \code{sparse}.
    }
  }
}
\examples{
data("crude")
tdm <- TermDocMatrix(crude)
removeSparseTerms(tdm, 0.2)
}
\keyword{methods}
