\name{Reader}
\alias{FunctionGenerator}
\alias{Reader}
\alias{getReaders}
\title{Readers}
\description{
  Creating readers.
}
\usage{
getReaders()
}
\details{
  \emph{Readers} are functions for extracting textual content and metadata out
  of elements delivered by a \code{\link{Source}}, and for constructing a
  \code{\link{TextDocument}}. A reader must accept following arguments in
  its signature:
  \describe{
    \item{\code{elem}}{a named list with the components \code{content} and
      \code{uri} (as delivered by a \code{\link{Source}} via
      \code{\link{getElem}} or \code{\link{pGetElem}}).}
    \item{\code{language}}{a character string giving the language.}
    \item{\code{id}}{a character giving a unique identifier for the created text
      document.}
  }
  The element \code{elem} is typically provided by a source whereas the language
  and the identifier are normally provided by a corpus constructor (for the case
  that \code{elem$content} does not give information on these two essential
  items).

  In case a reader expects configuration arguments we can use a function
  generator. A function generator is indicated by inheriting from class
  \code{FunctionGenerator} and \code{function}. It allows us to process
  additional arguments, store them in an environment, return a reader function
  with the well-defined signature described above, and still be able to access
  the additional arguments via lexical scoping. All corpus constructors in
  package \pkg{tm} check the reader function for being a function generator and
  if so apply it to yield the reader with the expected signature.
}
\value{
  For \code{getReaders()}, a character vector with readers provided by package
  \pkg{tm}.
}
\seealso{
  \code{\link{readDOC}}, \code{\link{readPDF}}, \code{\link{readPlain}},
  \code{\link{readRCV1}}, \code{\link{readRCV1asPlain}},
  \code{\link{readReut21578XML}}, \code{\link{readReut21578XMLasPlain}},
  and \code{\link{readXML}}.
}
