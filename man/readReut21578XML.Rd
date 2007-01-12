\name{readReut21578XML}
\alias{readReut21578XML}
\title{Read In A Reuters21578 XML Document}
\description{
  Returns a function which reads in a Reuters21578 XML document.
}
\usage{
readReut21578XML(...)
}
\arguments{
  \item{...}{arguments for the generator function.}
}
\details{
  Formally this function is a function generator, i.e., it returns a
  function (which reads in a Reuters21578 XML document) with a well-defined
  signature, but can access passed over arguments via lexical
  scoping. This is especially useful for reader functions for complex
  data structures which need a lot of configuration options.
}
\value{
   A \code{function} with the signature \code{elem, load,
    id}:
  \itemize{
    \item{elem}{A \code{list} with the two named elements
    \code{content} and \code{uri}. The first element must hold the
    document corpus to be read in, the second element must hold a
    call to the document corpus. The call is evaluated upon
    a request for load on demand.}
    \item{load}{A \code{logical} value indicating whether the
    document corpus should be immediately loaded into memory.}
    \item{id}{A \code{character} representing a unique
    identification string for the returned text document.}
  }

  The function returns a \code{XMLTextDocument} representing
  \code{content}.
}
\author{Ingo Feinerer}
\keyword{file}