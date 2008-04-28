\name{readXML}
\alias{readXML}
\title{Read In an XML Document}
\description{
  Returns a function which reads in an XML document. The structure of
  the XML document can be described with a so-called specification.
}
\usage{
readXML(spec, doc, ...)
}
\arguments{
  \item{spec}{a named \code{list} of \code{list}s each containing two
    \code{character} vectors. The constructed reader will map each list
    entry to a slot or meta datum corresponding to the named list
    entry. Valid names include \code{.Data} to access the document's
    content, any valid slot name, and characters which are mapped to
    \code{\link{LocalMetaData}} entries.

    Each list entry must consist of two character vectors: the first
    describes the type of the second argument, and the second is the
    specification entry. Valid combinations are:
    \describe{
      \item{type = "node", spec = "XPathExpression":}{the XPath
	expression \code{spec} extracts information from an XML node.}
      \item{type = "attribute", spec = "XPathExpression":}{the XPath
	expression \code{spec} extracts information from an attribute of
	an XML node.}
      \item{type = "function", spec = function(tree) ...:}{The function
	\code{spec} is called, passing over a tree representation (as
	delivered by \code{xmlInternalTreeParse} from package \pkg{XML})
	of the read in XML document as first argument.}
      \item{type = "unevaluated", spec = "String":}{the character vector
	\code{spec} is returned without modification.}
    }
  }
  \item{doc}{an (empty) document of some subclass of \code{\linkS4class{TextDocument}}}
  \item{...}{arguments for the generator function.}
}
\details{
  Formally this function is a function generator, i.e., it returns a
  function (which reads in a text document) with a well-defined
  signature, but can access passed over arguments (e.g., the specification)
  via lexical scoping.
}
\value{
  A \code{function} with the signature \code{elem, language, load, id}:

  \item{elem}{A \code{list} with the two named elements \code{content}
    and \code{uri}. The first element must hold the document to
    be read in, the second element must hold a call to extract this
    document. The call is evaluated upon a request for load on demand.}
  \item{load}{A \code{logical} value indicating whether the document
    corpus should be immediately loaded into memory.}
  \item{language}{A \code{character} vector giving the text's language.}
  \item{id}{A \code{character} vector representing a unique identification
    string for the returned text document.}

  The function returns \code{doc} augmented by the parsed information
  out of the XML file as described by \code{spec}.
}
\seealso{
  Vignette 'Extensions: How to Handle Custom File Formats'.

  Use \code{\link{getReaders}} to list available reader functions.
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
  doc = new("Reuters21578Document"))
}
}
\author{Ingo Feinerer}
