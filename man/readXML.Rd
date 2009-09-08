\name{readXML}
\alias{readXML}
\title{Read In an XML Document}
\description{
  Return a function which reads in an \acronym{XML} document. The structure of
  the \acronym{XML} document can be described with a so-called specification.
}
\usage{
readXML(spec, doc, \dots)
}
\arguments{
  \item{spec}{A named \code{list} of \code{list}s each containing two
    \code{character} vectors. The constructed reader will map each list
    entry to a attribute or meta datum corresponding to the named list
    entry. Valid names include \code{Content} to access the document's
    content, any valid attribute name, and characters which are mapped to
    \code{\link{LocalMetaData}} entries.

    Each list entry must consist of two character vectors: the first
    describes the type of the second argument, and the second is the
    specification entry. Valid combinations are:
    \describe{
      \item{type = "node", spec = "XPathExpression"}{The XPath
	expression \code{spec} extracts information from an \acronym{XML} node.}
      \item{type = "attribute", spec = "XPathExpression"}{The XPath
	expression \code{spec} extracts information from an attribute of
	an \acronym{XML} node.}
      \item{type = "function", spec = function(tree) \dots}{The function
	\code{spec} is called, passing over a tree representation (as
	delivered by \code{xmlInternalTreeParse} from package \pkg{XML})
	of the read in \acronym{XML} document as first argument.}
      \item{type = "unevaluated", spec = "String"}{The character vector
	\code{spec} is returned without modification.}
    }
  }
  \item{doc}{An (empty) document of some subclass of \code{TextDocument}}
  \item{\dots}{Arguments for the generator function.}
}
\details{
  Formally this function is a function generator, i.e., it returns a
  function (which reads in a text document) with a well-defined
  signature, but can access passed over arguments (e.g., the specification)
  via lexical scoping.
}
\value{
  A \code{function} with the signature \code{elem, language, id}:
  \item{elem}{A \code{list} with the two named elements \code{content}
    and \code{uri}. The first element must hold the document to
    be read in, the second element must hold a call to extract this
    document. The call is evaluated upon a request for load on demand.}
  \item{language}{A \code{character} vector giving the text's language.}
  \item{id}{A \code{character} vector representing a unique identification
    string for the returned text document.}
  The function returns \code{doc} augmented by the parsed information
  out of the \acronym{XML} file as described by \code{spec}.
}
\seealso{
  Vignette 'Extensions: How to Handle Custom File Formats'.

  \code{\link{getReaders}} to list available reader functions.
}
\examples{
\dontrun{
readReut21578XML <- readXML(
  spec = list(Author = list("node", "/REUTERS/TEXT/AUTHOR"),
              DateTimeStamp = list("function", function(node)
                strptime(sapply(XML::getNodeSet(node, "/REUTERS/DATE"), XML::xmlValue),
                         format = "%d-%B-%Y %H:%M:%S",
                         tz = "GMT")),
              Description = list("unevaluated", ""),
              Heading = list("node", "/REUTERS/TEXT/TITLE"),
              ID = list("attribute", "/REUTERS/@NEWID"),
              Origin = list("unevaluated", "Reuters-21578 XML"),
              Topics = list("node", "/REUTERS/TOPICS/D")),
  doc = Reuters21578Document())
}
}
\author{Ingo Feinerer}
