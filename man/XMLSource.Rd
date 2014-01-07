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
  \item{x}{a file name or a connection.}
  \item{parser}{a function accepting an \acronym{XML} tree (as delivered by
    \code{xmlTreeParse} in package \pkg{XML}) as input and returning a
    list of \acronym{XML} elements.}
  \item{reader}{a function capable of turning \acronym{XML} elements as
    returned by \code{parser} into a subclass of \code{\link{TextDocument}}.}
  \item{encoding}{encoding to be assumed for input strings. It is used to mark
    character strings as known to be in Latin-1 or UTF-8: it is not used to
    re-encode the input.}
}
\value{
  An object of class \code{XMLSource} which extends the class
  \code{\link{Source}} representing an \acronym{XML} file.
}
\seealso{
  Vignette 'Extensions: How to Handle Custom File Formats',
  \code{\link{readXML}};
  \code{\link{Encoding}} 
}
\examples{
## An implementation for readGmane is provided as an example in ?readXML
example(readXML)

## Construct a source for a Gmane mailing list RSS feed.
GmaneSource <-
function(x, encoding = "unknown")
  XMLSource(x, function(tree)
               XML::xmlChildren(XML::xmlRoot(tree))
                 [names(XML::xmlChildren(XML::xmlRoot(tree))) == "item"],
            readGmane, encoding)

\dontrun{gs <- GmaneSource("http://rss.gmane.org/gmane.comp.lang.r.general")
elem <- getElem(stepNext(gs))
(gmane <- readGmane(elem, language = "en", id = "id1"))
meta(gmane)}
}
\author{Ingo Feinerer}
