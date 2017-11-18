\name{Source}
\alias{Source}
\alias{SimpleSource}
\alias{close.SimpleSource}
\alias{eoi}
\alias{eoi.SimpleSource}
\alias{getMeta}
\alias{getMeta.DataframeSource}
\alias{getElem}
\alias{getElem.DataframeSource}
\alias{getElem.DirSource}
\alias{getElem.URISource}
\alias{getElem.VectorSource}
\alias{getElem.XMLSource}
\alias{getSources}
\alias{length.SimpleSource}
\alias{open.SimpleSource}
\alias{pGetElem}
\alias{pGetElem.DataframeSource}
\alias{pGetElem.DirSource}
\alias{pGetElem.URISource}
\alias{pGetElem.VectorSource}
\alias{reader}
\alias{reader.SimpleSource}
\alias{Source}
\alias{stepNext}
\alias{stepNext.SimpleSource}
\title{Sources}
\description{
  Creating and accessing sources.
}
\usage{
SimpleSource(encoding = "",
             length = 0,
             position = 0,
             reader = readPlain,
             \dots,
             class)
getSources()
\method{close}{SimpleSource}(con, \dots)
\method{eoi}{SimpleSource}(x)
\method{getMeta}{DataframeSource}(x)
\method{getElem}{DataframeSource}(x)
\method{getElem}{DirSource}(x)
\method{getElem}{URISource}(x)
\method{getElem}{VectorSource}(x)
\method{getElem}{XMLSource}(x)
\method{length}{SimpleSource}(x)
\method{open}{SimpleSource}(con, \dots)
\method{pGetElem}{DataframeSource}(x)
\method{pGetElem}{DirSource}(x)
\method{pGetElem}{URISource}(x)
\method{pGetElem}{VectorSource}(x)
\method{reader}{SimpleSource}(x)
\method{stepNext}{SimpleSource}(x)
}
\arguments{
  \item{x}{A \code{Source}.}
  \item{con}{A \code{Source}.}
  \item{encoding}{a character giving the encoding of the elements delivered by
    the source.}
  \item{length}{a non-negative integer denoting the number of elements delivered
    by the source. If the length is unknown in advance set it to \code{0}.}
  \item{position}{a numeric indicating the current position in the source.}
  \item{reader}{a reader function (generator).}
  \item{\dots}{For \code{SimpleSource} tag-value pairs for storing additional
    information; not used otherwise.}
  \item{class}{a character vector giving additional classes to be used for the
    created source.}
}
\details{
  \emph{Sources} abstract input locations, like a directory, a connection, or
  simply an \R vector, in order to acquire content in a uniform way. In packages
  which employ the infrastructure provided by package \pkg{tm}, such sources are
  represented via the virtual S3 class \code{Source}: such packages then provide
  S3 source classes extending the virtual base class (such as
  \code{\link{DirSource}} provided by package \pkg{tm} itself).

  All extension classes must provide implementations for the functions
  \code{close}, \code{eoi}, \code{getElem}, \code{length}, \code{open},
  \code{reader}, and \code{stepNext}. For parallel element access the
  (optional) function \code{pGetElem} must be provided as well. If
  document level metadata is available, the (optional) function \code{getMeta}
  must be implemented.

  The functions \code{open} and \code{close} open and close the source,
  respectively. \code{eoi} indicates end of input. \code{getElem} fetches the
  element at the current position, whereas \code{pGetElem} retrieves all
  elements in parallel at once. The function \code{length} gives the number of
  elements. \code{reader} returns a default reader for processing elements.
  \code{stepNext} increases the position in the source to acquire the next
  element.

  The function \code{SimpleSource} provides a simple reference implementation
  and can be used when creating custom sources.
}
\value{
  For \code{SimpleSource}, an object inheriting from \code{class},
  \code{SimpleSource}, and \code{Source}.

  For \code{getSources}, a character vector with sources provided by package
  \pkg{tm}.

  \code{open} and \code{close} return the opened and closed source,
  respectively.

  For \code{eoi}, a logical indicating if the end of input of the source is
  reached.

  For \code{getElem} a named list with the components \code{content} holding the
  document and \code{uri} giving a uniform resource identifier (e.g., a file
  path or \acronym{URL}; \code{NULL} if not applicable or unavailable). For
  \code{pGetElem} a list of such named lists.

  For \code{length}, an integer for the number of elements.

  For \code{reader}, a function for the default reader.
}
\seealso{
  \code{\link{DataframeSource}}, \code{\link{DirSource}},
  \code{\link{URISource}}, \code{\link{VectorSource}}, and
  \code{\link{XMLSource}}.
}
