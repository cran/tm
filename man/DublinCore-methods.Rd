\name{DublinCore-methods}
\docType{methods}
\alias{DublinCore}
\alias{DublinCore<-}
\alias{DublinCore-methods}
\alias{DublinCore,TextDocument-method}
\alias{DublinCore<-,TextDocument-method}
\title{Methods for Function DublinCore in Package `tm'}
\description{
  Methods for function \code{DublinCore} in package \pkg{tm}, which provide
  a bijective mapping between Simple Dublin Core meta data and \pkg{tm} meta
  data structures.
}
\section{Methods}{
  \describe{
    \item{object = "TextDocument", tag = NULL}{Returns or sets the
      Simple Dublin Core meta datum named \code{tag} for
      \code{object}. \code{tag} must be a valid Simple Dublin Core
      element name (i.e, Title, Creator, Subject, Description,
      Publisher, Contributor, Date, Type, Format, Identifier, Source,
      Language, Relation, Coverage, or Rights) or \code{NULL}. For the
      latter all Dublin Core meta data are printed.}
  }
}
\examples{
data("crude")
DublinCore(crude[[1]])
meta(crude[[1]])
DublinCore(crude[[1]], tag = "Creator") <- "Ano Nymous"
DublinCore(crude[[1]], tag = "Format") <- "XML"
DublinCore(crude[[1]])
meta(crude[[1]])
}
\keyword{methods}
