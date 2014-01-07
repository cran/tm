\name{readXML}
\alias{readXML}
\title{Read In an XML Document}
\description{
  Return a function which reads in an \acronym{XML} document. The structure of
  the \acronym{XML} document can be described with a specification.
}
\usage{
readXML(spec, doc)
}
\arguments{
  \item{spec}{A named list of lists each containing two
    components. The constructed reader will map each list
    entry to an attribute or meta datum corresponding to the named list
    entry. Valid names include \code{Content} to access the document's
    content, any valid attribute name, and characters which are mapped to
    \code{\link{LocalMetaData}} entries.

    Each list entry must consist of two components: the first must be a string
    describing the type of the second argument, and the second is the
    specification entry. Valid combinations are:
    \describe{
      \item{\code{type = "node", spec = "XPathExpression"}}{The XPath
	expression \code{spec} extracts information from an \acronym{XML} node.}
      \item{\code{type = "attribute", spec = "XPathExpression"}}{The XPath
	expression \code{spec} extracts information from an attribute of
	an \acronym{XML} node.}
      \item{\code{type = "function", spec = function(tree) \dots}}{The function
	\code{spec} is called, passing over a tree representation (as
	delivered by \code{xmlInternalTreeParse} from package \pkg{XML})
	of the read in \acronym{XML} document as first argument.}
      \item{\code{type = "unevaluated", spec = "String"}}{The character vector
	\code{spec} is returned without modification.}
    }
  }
  \item{doc}{An (empty) document of some subclass of \code{TextDocument}}
}
\details{
  Formally this function is a function generator, i.e., it returns a
  function (which reads in a text document) with a well-defined
  signature, but can access passed over arguments (e.g., the specification)
  via lexical scoping.
}
\value{
  A function with the signature \code{elem, language, id}:
  \describe{
    \item{\code{elem}}{a list with the named component \code{content} which
      must hold the document to be read in.}
    \item{\code{language}}{a string giving the text's language.}
    \item{\code{id}}{a unique identification string for the returned text
      document.}
  }
  The function returns \code{doc} augmented by the parsed information
  as described by \code{spec} out of the \acronym{XML} file in
  \code{elem$content}.
}
\seealso{
  Vignette 'Extensions: How to Handle Custom File Formats',
  \code{\link{XMLSource}}.

  \code{\link{getReaders}} to list available reader functions.
}
\examples{
readGmane <-
readXML(spec = list(Author = list("node", "/item/creator"),
                    Content = list("node", "/item/description"),
                    DateTimeStamp = list("function", function(node)
                    strptime(sapply(XML::getNodeSet(node, "/item/date"), XML::xmlValue),
                             format = "\%Y-\%m-\%dT\%H:\%M:\%S",
                             tz = "GMT")),
                    Description = list("unevaluated", ""),
                    Heading = list("node", "/item/title"),
                    ID = list("node", "/item/link"),
                    Origin = list("unevaluated", "Gmane Mailing List Archive")),
                    doc = PlainTextDocument())
}
\author{Ingo Feinerer}
