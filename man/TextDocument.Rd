\name{TextDocument}
\alias{TextDocument}
\alias{Author}
\alias{Content}
\alias{Content<-}
\alias{Content<-.PlainTextDocument}
\alias{Content<-.XMLDocument}
\alias{DateTimeStamp}
\alias{Description}
\alias{Heading}
\alias{ID}
\alias{Origin}
\alias{Language}
\alias{LocalMetaData}
\title{Access and Modify Text Documents}
\description{
  Access the meta data of text documents, and access and modify the
  content of text documents.
}
\details{
  The class \code{TextDocument} provides an abstraction over the concept
  of text documents and attached meta data which is stored in following
  attributes:
  \describe{
    \item{\code{Author}}{Object of class \code{character} containing
      the author names.}
    \item{\code{DateTimeStamp}}{Object of class \code{POSIXlt} containing the
      date and time when the document was written.}
    \item{\code{Description}}{Object of class \code{character} containing
      additional text information.}
    \item{\code{ID}}{Object of class \code{character} containing
      an identifier.}
    \item{\code{Origin}}{Object of class \code{character} containing
      information on the source and origin of the text.}
    \item{\code{Heading}}{Object of class \code{character} containing
      the title or a short heading.}
    \item{\code{Language}}{Object of class \code{character} containing
      the language of the text.}
    \item{\code{LocalMetaData}}{Object of class \code{list} containing
      additional meta data in form of tag-value pairs which is local to
      each individual text document.}
  }
}
\seealso{
  \code{\link{meta}} and \code{\link{DublinCore}} which provide a
  unified interface for meta data management.
}
\author{Ingo Feinerer}
