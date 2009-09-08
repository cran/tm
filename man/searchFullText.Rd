\name{searchFullText}
\alias{searchFullText}
\alias{searchFullText.PlainTextDocument}
\title{Full Text Search}
\description{
  Perform a full text search in text documents.
}
\usage{
\method{searchFullText}{PlainTextDocument}(x, pattern)
}
\arguments{
  \item{x}{A text document.}
  \item{pattern}{A regular expression as accepted by \code{\link{gsub}}.}
}
\value{
  \code{TRUE} if the regular expression \code{pattern} matches in
  \code{x}, otherwise \code{FALSE}.
}
\seealso{
  \code{\link{getFilters}} to list available filter functions.
}
\examples{
data("crude")
searchFullText(crude[[1]], "co[m]?pany")
}
