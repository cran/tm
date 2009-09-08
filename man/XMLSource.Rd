\name{XMLSource}
\alias{XMLSource}
\title{XML Source}
\description{
  Constructs a source for an \acronym{XML} file.
}
\usage{
XMLSource(x, parser, reader, encoding = "UTF-8")
}
\arguments{
  \item{x}{Either a \code{character} identifying a file or a
    \code{connection}.}
  \item{parser}{A \code{function} accepting an \acronym{XML} tree (as delivered by
    \code{xmlTreeParse} in package \pkg{XML}) as input and returning a
    list of \acronym{XML} elements.}
  \item{reader}{A \code{function} capable of turning \acronym{XML} elements as
    returned by \code{parser} into a subclass of
    \code{\link{TextDocument}}.}
  \item{encoding}{A \code{character} giving the encoding of \code{x}.}
}
\value{
  An object of class \code{XMLSource} which extends the class
  \code{\link{Source}} representing an \acronym{XML} file.
}
\seealso{
  Vignette 'Extensions: How to Handle Custom File Formats'.
}
\author{Ingo Feinerer}
