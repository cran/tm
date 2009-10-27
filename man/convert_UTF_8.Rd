\name{convert_UTF_8}
\alias{convert_UTF_8}
\title{Convert Encoding to UTF-8}
\description{
  Convert the encoding of a character or text document to UTF-8.
}
\usage{
convert_UTF_8(x, from = "", sub = NA)
}
\arguments{
  \item{x}{A character or text document.}
  \item{from}{A character string describing the current encoding.}
  \item{sub}{A character string. If not 'NA' it is used to replace any
    non-convertible bytes in the input.}
}
\value{
  The character or text document \code{x} converted to UTF-8 encoding.
}
\details{
  This function internally uses \code{\link{iconv}} for conversion, and
  the arguments \code{from} and \code{sub} are passed over.
}
\seealso{
  \code{\link{iconv}}

  Use \code{\link{getTransformations}} to list available transformation (mapping) functions.
}
\examples{
data("crude")
convert_UTF_8(crude[[1]])
}
\author{Ingo Feinerer}
