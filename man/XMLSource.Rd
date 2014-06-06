\name{XMLSource}
\alias{XMLSource}
\title{XML Source}
\description{
  Create an \acronym{XML} source.
}
\usage{
XMLSource(x, parser, reader)
}
\arguments{
  \item{x}{a character giving a uniform resource identifier.}
  \item{parser}{a function accepting an \acronym{XML} tree (as delivered by
    \code{\link[XML]{xmlTreeParse}} in package \pkg{XML}) as input and returning
    a list of \acronym{XML} elements.}
  \item{reader}{a function capable of turning \acronym{XML} elements as
    returned by \code{parser} into a subclass of \code{\link{TextDocument}}.}
}
\value{
  An object inheriting from \code{XMLSource}, \code{\link{SimpleSource}},
  and \code{\link{Source}}.
}
\seealso{
  \code{\link{Source}} for basic information on the source infrastructure
  employed by package \pkg{tm}.

  Vignette 'Extensions: How to Handle Custom File Formats', and
  \code{\link{readXML}}.
}
\examples{
## An implementation for readGmane is provided as an example in ?readXML
example(readXML)

## Construct a source for a Gmane mailing list RSS feed.
GmaneSource <-
function(x)
    XMLSource(x,
              function(tree) {
                  nodes <- XML::xmlChildren(XML::xmlRoot(tree))
                  nodes[names(nodes) == "item"]
              },
              readGmane)
\dontrun{gs <- GmaneSource("http://rss.gmane.org/gmane.comp.lang.r.general")
elem <- getElem(stepNext(gs))
(gmane <- readGmane(elem, language = "en", id = "id1"))
meta(gmane)}
}
