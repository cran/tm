\name{readTagged}
\alias{readTagged}
\title{Read In a POS-Tagged Word Text Document}
\description{
  Return a function which reads in a text document containing POS-tagged words.
}
\usage{
readTagged(\dots)
}
\arguments{
  \item{\dots}{Arguments passed to \code{\link[NLP]{TaggedTextDocument}}.}
}
\details{
  Formally this function is a function generator, i.e., it returns a
  function (which reads in a text document) with a well-defined
  signature, but can access passed over arguments (\code{\dots})
  via lexical scoping.
}
\value{
  A \code{function} with the following formals:
  \describe{
    \item{\code{elem}}{a named list with the component \code{content} which must
      hold the document to be read in or the component \code{uri} holding a
      connection object or a character string.}
    \item{\code{language}}{a string giving the language.}
    \item{\code{id}}{a character giving a unique identifier for the created
      text document.}
  }
  The function returns a \code{\link[NLP]{TaggedTextDocument}} representing the
  text and metadata extracted from \code{elem$content} or \code{elem$uri}. The
  argument \code{id} is used as fallback if \code{elem$uri} is null.
}
\seealso{
  \code{\link{Reader}} for basic information on the reader infrastructure
  employed by package \pkg{tm}.
}
\examples{
# See http://www.nltk.org/book/ch05.html or file ca01 in the Brown corpus
x <- paste("The/at grand/jj jury/nn commented/vbd on/in a/at number/nn of/in",
           "other/ap topics/nns ,/, among/in them/ppo the/at Atlanta/np and/cc",
           "Fulton/np-tl County/nn-tl purchasing/vbg departments/nns which/wdt",
           "it/pps said/vbd ``/`` are/ber well/ql operated/vbn and/cc follow/vb",
           "generally/rb accepted/vbn practices/nns which/wdt inure/vb to/in the/at",
           "best/jjt interest/nn of/in both/abx governments/nns ''/'' ./.")
vs <- VectorSource(x)
elem <- getElem(stepNext(vs))
(doc <- readTagged()(elem, language = "en", id = "id1"))
tagged_words(doc)
}
