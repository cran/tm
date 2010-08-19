\name{tm_tag_score}
\alias{tm_tag_score}
\alias{tm_tag_score.numeric}
\alias{tm_tag_score.PlainTextDocument}
\alias{tm_tag_score.TermDocumentMatrix}
\title{Compute a Tag Score}
\description{
  Compute a score based on the number of tags found in a document.
}
\usage{
\method{tm_tag_score}{numeric}(x, tags, FUN)
\method{tm_tag_score}{PlainTextDocument}(x, tags, FUN = function(x) sum(x, na.rm = TRUE))
\method{tm_tag_score}{TermDocumentMatrix}(x, tags, FUN = slam::col_sums)
}
\arguments{
  \item{x}{Either a \code{\link{PlainTextDocument}}, a named numeric as
    returned by \code{\link{termFreq}}, or a
    \code{\link{TermDocumentMatrix}}.}
  \item{tags}{A character vector of tags to be matched.}
  \item{FUN}{A function computing a score from the number of tags
    matching in \code{x}.}
}
\value{
  A score as computed by \code{FUN} from the number of matching
  \code{tags} in \code{x}.
}
\examples{
data("acq")
tm_tag_score(acq[[1]], c("company", "change"))
\dontrun{
## Test for positive and negative sentiments
require("tm.plugin.tags")
sapply(acq[1:10], tm_tag_score, tm_get_tags("Positiv"))
sapply(acq[1:10], tm_tag_score, tm_get_tags("Negativ"))
tm_tag_score(TermDocumentMatrix(acq[1:10],
                                control = list(removePunctuation = TRUE)),
             tm_get_tags("Positiv"))
}
}
