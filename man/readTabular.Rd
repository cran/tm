\name{readTabular}
\alias{readTabular}
\title{Read In a Text Document}
\description{
  Return a function which reads in a text document from a tabular data
  structure (like a data frame or a list matrix) with knowledge about
  its internal structure and possible available metadata as specified by
  a so-called mapping.
}
\usage{
readTabular(mapping)
}
\arguments{
  \item{mapping}{A named list of characters. The
    constructed reader will map each character entry to the content or
    metadatum of the text document as specified by the named list
    entry. Valid names include \code{content} to access the document's
    content, and character strings which are mapped to metadata entries.}
}
\details{
  Formally this function is a function generator, i.e., it returns a
  function (which reads in a text document) with a well-defined
  signature, but can access passed over arguments (e.g., the mapping)
  via lexical scoping.
}
\value{
  A \code{function} with the following formals:
  \describe{
    \item{\code{elem}}{a named list with the component \code{content} which must
      hold the document to be read in.}
    \item{\code{language}}{a string giving the language.}
    \item{\code{id}}{a character giving a unique identifier for the created
      text document.}
  }
  The function returns a \code{\link{PlainTextDocument}} representing the text
  and metadata extracted from \code{elem$content}. The arguments \code{language}
  and \code{id} are used as fallback if no corresponding metadata entries are
  found in \code{elem$content}.
}
\seealso{
  \code{\link{Reader}} for basic information on the reader infrastructure
  employed by package \pkg{tm}.

  Vignette 'Extensions: How to Handle Custom File Formats'.
}
\examples{
df <- data.frame(contents = c("content 1", "content 2", "content 3"),
                 title    = c("title 1"  , "title 2"  , "title 3"  ),
                 authors  = c("author 1" , "author 2" , "author 3" ),
                 topics   = c("topic 1"  , "topic 2"  , "topic 3"  ),
                 stringsAsFactors = FALSE)
m <- list(content = "contents", heading = "title",
          author = "authors", topic = "topics")
myReader <- readTabular(mapping = m)
ds <- DataframeSource(df)
elem <- getElem(stepNext(ds))
(result <- myReader(elem, language = "en", id = "id1"))
meta(result)
}
