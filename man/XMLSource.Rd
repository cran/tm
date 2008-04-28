\name{XMLSource}
\alias{XMLSource}
\title{XML Source}
\description{
  Constructs a source for an XML file.
}
\usage{
XMLSource(object, parser, reader, encoding = "UTF-8")
}
\arguments{
  \item{object}{either a \code{character} identifying the file or a
    \code{connection}.}
  \item{parser}{a \code{function} accepting an XML tree (as delivered by
    \code{xmlTreeParse} in package \pkg{XML}) as input and returning a
    list of XML elements.}
  \item{reader}{a \code{function} capable of turning XML elements as
    returned by \code{parser} into a subclass of
    \code{\linkS4class{TextDocument}}.}
  \item{encoding}{a \code{character} giving the encoding of the file.}
}
\value{
  An S4 object of class \code{\linkS4class{XMLSource}} which extends the
  class \code{\linkS4class{Source}} representing an XML file.
}
\seealso{
  Vignette 'Extensions: How to Handle Custom File Formats'.
}
\author{Ingo Feinerer}
