\name{tmUpdate-methods}
\docType{methods}
\alias{tmUpdate}
\alias{tmUpdate-methods}
\alias{tmUpdate,Corpus,DirSource-method}
\title{Methods for Function tmUpdate in Package `tm'}
\description{
Methods for function \code{tmUpdate} in package \pkg{tm}.
}
\section{Methods}{
  \describe{
    \item{object = "Corpus", origin = "DirSource", readerControl = list(reader = origin@DefaultReader, language = "en\_US", load = TRUE)}{Checks whether
      new documents are available in \code{origin}. If, \code{object} is
      updated, i.e., it is augmented with the newly available
      items. \code{readerControl} specifies arguments to read in new
      elements provided by \code{origin}.}
  }
}
\keyword{methods}
