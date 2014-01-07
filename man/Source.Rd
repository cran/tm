\name{Source}
\alias{Source}
\alias{eoi}
\alias{eoi.Source}
\alias{getElem}
\alias{getElem.DataframeSource}
\alias{getElem.DirSource}
\alias{getElem.URISource}
\alias{getElem.VectorSource}
\alias{getElem.XMLSource}
\alias{pGetElem}
\alias{pGetElem.DataframeSource}
\alias{pGetElem.DirSource}
\alias{is.Source}
\alias{pGetElem.VectorSource}
\alias{Source}
\alias{stepNext}
\alias{stepNext.Source}
\title{Create and Access Sources}
\description{
  Create and access sources which abstract input locations, like a
  directory, a connection, or simply an \R vector.
}
\usage{
Source(defaultReader = readPlain, encoding = "unknown", length = NA_integer_,
       names = NA_character_, position = 0, vectorized = TRUE, class)
is.Source(x)
\method{eoi}{Source}(x)
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
  \item{x}{a source.}
  \item{defaultReader}{a reader function (generator).}
  \item{encoding}{a character specifying the encoding of the elements
    delivered by the source.}
  \item{length}{an integer denoting the number of elements delivered by the
    source. If the length is unknown in advance it must be set to
    \code{NA_integer_}.}
  \item{names}{a character vector giving element names.}
  \item{position}{a numeric indicating the position in the source.}
  \item{vectorized}{a logical indicating the ability for parallel element
    access.}
  \item{class}{a character vector for extending the class attribute.}
}
\details{
  The function \code{Source} is a constructor and should be used when creating
  custom sources. Internally a source is represented as a \code{list} with the
  class attribute \code{Source}. Custom sources may extend the internal list
  structure with additional named components.

  The function \code{is.Source} returns \code{TRUE} for a valid source and
  \code{FALSE} otherwise.

  Each source must provide implementations for the three interface functions
  \code{eoi}, \code{getElem}, and \code{stepNext}. If \code{vectorized} is set
  \code{pGetElem} must be implemented as well. The function \code{eoi} returns
  \code{TRUE} if the end of input of the source is reached. \code{getElem}
  fetches the element at the current position, whereas \code{pGetElem} retrieves
  all elements in parallel at once. Retrieved elements must be encapsulated in a
  list with the named components \code{content} holding the document and
  \code{uri} pointing to the origin of the document (e.g., a file path or a
  connection; \code{NA} if not applicable or unavailable). \code{stepNext}
  increases the position in the source to the next element.
}
\seealso{
  \code{\link{getSources}} to list available sources;
  \code{\link{readPlain}};
  \code{\link{Encoding}}.
}
\author{Ingo Feinerer}
