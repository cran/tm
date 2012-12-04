\name{XMLSource}
\alias{XMLSource}
\title{XML Source}
\description{
  Constructs a source for an \acronym{XML} file.
}
\usage{
XMLSource(x, parser, reader, encoding = "unknown")
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
  \item{encoding}{encoding to be assumed for input strings.  It is used to mark
    character strings as known to be in Latin-1 or UTF-8: it is not used to
    re-encode the input.}
}
\value{
  An object of class \code{XMLSource} which extends the class
  \code{\link{Source}} representing an \acronym{XML} file.
}
\seealso{
  Vignette 'Extensions: How to Handle Custom File Formats'. \code{\link{Encoding}} on encodings in \R.
}
\author{Ingo Feinerer}
