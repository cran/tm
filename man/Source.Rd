\name{Source}
\alias{Source}
\alias{eoi}
\alias{eoi.DataframeSource}
\alias{eoi.DirSource}
\alias{eoi.URISource}
\alias{eoi.VectorSource}
\alias{eoi.XMLSource}
\alias{getElem}
\alias{getElem.DataframeSource}
\alias{getElem.DirSource}
\alias{getElem.URISource}
\alias{getElem.VectorSource}
\alias{getElem.XMLSource}
\alias{pGetElem}
\alias{pGetElem.DataframeSource}
\alias{pGetElem.DirSource}
\alias{pGetElem.VectorSource}
\alias{stepNext}
\alias{stepNext.Source}
\title{Access Sources}
\description{
  Methods to access sources which abstract input locations, like a
  directory, a connection, or simply an \R vector.
}
\usage{
\method{eoi}{DataframeSource}(x)
\method{eoi}{DirSource}(x)
\method{eoi}{URISource}(x)
\method{eoi}{VectorSource}(x)
\method{eoi}{XMLSource}(x)
\method{getElem}{DataframeSource}(x)
\method{getElem}{DirSource}(x)
\method{getElem}{URISource}(x)
\method{getElem}{VectorSource}(x)
\method{getElem}{XMLSource}(x)
\method{pGetElem}{DataframeSource}(x)
\method{pGetElem}{DirSource}(x)
\method{pGetElem}{VectorSource}(x)
\method{stepNext}{Source}(x)
}
\arguments{
  \item{x}{A source.}
}
\details{
  The class \code{Source} is implemented as a \code{list} with
  following components:
  \describe{
    \item{\code{DefaultReader}}{Object of class \code{function}
      holding a default reader.}
    \item{\code{Encoding}}{Object of class \code{character} holding the
      encoding of the texts delivered by the source.}
    \item{\code{Length}}{Object of class \code{numeric} denoting the
      number of the elements delivered by the source. If the number
      cannot be determined in advance it is set to zero.}
    \item{\code{LoDSupport}}{Object of class \code{logical} indicating
      whether this source supports load on demand.}
    \item{\code{Position}}{object of class \code{numeric} indicating
      the position in the source.}
    \item{\code{Vectorized}}{object of class \code{logical} indicating
      the ability for parallel element access.}
  }

  The function \code{eoi} returns \code{TRUE} if the end of input of the
  source is reached. \code{getElem} fetches the element at the
  current position, whereas \code{pGetElem} retrieves all elements in
  parallel at once. \code{stepNext} increases the position in the source
  to the next element.
}
\seealso{
  \code{\link{getSources}} to list available sources.
}
\author{Ingo Feinerer}
