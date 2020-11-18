\name{removeNumbers}
\alias{removeNumbers}
\alias{removeNumbers.character}
\alias{removeNumbers.PlainTextDocument}
\title{Remove Numbers from a Text Document}
\description{
  Remove numbers from a text document.
}
\usage{
\method{removeNumbers}{character}(x, ucp = FALSE, \dots)
\method{removeNumbers}{PlainTextDocument}(x, \dots)
}
\arguments{
  \item{x}{a character vector or text document.}
  \item{ucp}{a logical specifying whether to use Unicode character
    properties for determining digit characters.  If \code{FALSE} 
    (default), characters in the ASCII \code{[:digit:]} class (i.e., the
    decimal digits from 0 to 9) are taken; if \code{TRUE}, the
    characters with Unicode general category \code{Nd} (Decimal_Number).}
  \item{\dots}{arguments to be passed to or from methods;
    in particular, from the \code{PlainTextDocument} method to the
    \code{character} method.}
}
\value{
  The text document without numbers.
}
\seealso{
  \code{\link{getTransformations}} to list available transformation
  (mapping) functions.

  \url{https://unicode.org/reports/tr44/#General_Category_Values}.
}
\examples{
data("crude")
crude[[1]]
removeNumbers(crude[[1]])
}
