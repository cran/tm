\name{readTabular}
\alias{readTabular}
\title{Read In a Text Document}
\description{
  Returns a function which reads in a text document from a tabular data
  structure (like a data frame or a list matrix) with knowledge about
  its internal structure and possible available metadata as specified by
  so-called mappings.
}
\usage{
readTabular(mappings, \dots)
}
\arguments{
  \item{mappings}{a named \code{list} of \code{character}s. The
    constructed reader will map each character entry to a slot or meta
    datum corresponding to the named list entry. Valid names include
    \code{.Data} to access the document's content, any valid slot name,
    and characters which are mapped to \code{\link{LocalMetaData}}
    entries.}
  \item{\dots}{arguments for the generator function.}
}
\details{
  Formally this function is a function generator, i.e., it returns a
  function (which reads in a text document) with a well-defined
  signature, but can access passed over arguments (e.g., the mappings)
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

  The function returns a \code{PlainTextDocument} representing
  \code{content}.
}
\seealso{
  Vignette 'Extensions: How to Handle Custom File Formats'.

  Use \code{\link{getReaders}} to list available reader functions.
}
\examples{
df <- data.frame(contents = c("content 1", "content 2", "content 3"),
                 title    = c("title 1"  , "title 2"  , "title 3"  ),
                 authors  = c("author 1" , "author 2" , "author 3" ),
                 topics   = c("topic 1"  , "topic 2"  , "topic 3"  ),
                 stringsAsFactors = FALSE)
m <- list(.Data = "contents", Heading = "title",
          Author = "authors", Topic = "topics")
myReader <- readTabular(mappings = m)
ds <- DataframeSource(df)
elem <- getElem(stepNext(ds))
(result <- myReader(elem, load = TRUE, language = "en", id = "id1"))
meta(result)
}
\author{Ingo Feinerer}
