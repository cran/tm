\name{XMLSource}
\alias{XMLSource}
\title{XML Source}
\description{
  Create an \acronym{XML} source.
}
\usage{
XMLSource(x, parser = xml_contents, reader)
}
\arguments{
  \item{x}{a character giving a uniform resource identifier.}
  \item{parser}{a function accepting an \acronym{XML} document (as delivered by
    \code{\link[xml2]{read_xml}} in package \pkg{xml2}) as input and returning
    \acronym{XML} elements/nodes.}
  \item{reader}{a function capable of turning \acronym{XML} elements/nodes as
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
