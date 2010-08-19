\name{getTransformations}
\alias{getTransformations}
\title{List Available Transformations}
\description{
  List available transformations (mappings) which can be used with
  \code{\link{tm_map}}.
}
\usage{
getTransformations()
}
\value{
  A character vector with available transformations.
}
\author{Ingo Feinerer}
\seealso{
  \code{\link{as.PlainTextDocument}},
  \code{\link{removeNumbers}},
  \code{\link{removePunctuation}},
  \code{\link{removeWords}},
  \code{\link{stemDocument}},
  \code{\link{stripWhitespace}}.
}  
\examples{
getTransformations()
}
