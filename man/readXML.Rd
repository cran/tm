\name{readXML}
\alias{readXML}
\title{Read In an XML Document}
\description{
  Return a function which reads in an \acronym{XML} document. The structure of
  the \acronym{XML} document is described with a specification.
}
\usage{
readXML(spec, doc)
}
\arguments{
  \item{spec}{A named list of lists each containing two components. The
    constructed reader will map each list entry to the content or metadatum of
    the text document as specified by the named list entry. Valid names include
    \code{content} to access the document's content, and character strings which
    are mapped to metadata entries.

    Each list entry must consist of two components: the first must be a string
    describing the type of the second argument, and the second is the
    specification entry. Valid combinations are:
    \describe{
      \item{\code{type = "node", spec = "XPathExpression"}}{The XPath (1.0)
        expression \code{spec} extracts information from an \acronym{XML} node.}
      \item{\code{type = "function", spec = function(doc) \dots}}{The function
        \code{spec} is called, passing over the \acronym{XML} document (as
        delivered by \code{\link[xml2]{read_xml}} from package \pkg{xml2}) as
        first argument.}
      \item{\code{type = "unevaluated", spec = "String"}}{The character vector
        \code{spec} is returned without modification.}
    }
  }
  \item{doc}{An (empty) document of some subclass of \code{TextDocument}.}
}
\details{
  Formally this function is a function generator, i.e., it returns a
  function (which reads in a text document) with a well-defined
  signature, but can access passed over arguments (e.g., the specification)
  via lexical scoping.
}
\value{
  A function with the following formals:
  \describe{
    \item{\code{elem}}{a named list with the component \code{content} which
      must hold the document to be read in.}
    \item{\code{language}}{a string giving the language.}
    \item{\code{id}}{a character giving a unique identifier for the created
      text document.}
  }
  The function returns \code{doc} augmented by the parsed information
  as described by \code{spec} out of the \acronym{XML} file in
  \code{elem$content}. The arguments \code{language} and \code{id} are used as
  fallback: \code{language} if no corresponding metadata entry is found in
  \code{elem$content}, and \code{id} if no corresponding metadata entry is found
  in \code{elem$content} and if \code{elem$uri} is null.
}
\seealso{
  \code{\link{Reader}} for basic information on the reader infrastructure
  employed by package \pkg{tm}.

  Vignette 'Extensions: How to Handle Custom File Formats', and
  \code{\link{XMLSource}}.
}
