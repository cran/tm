\name{getTransformations}
\alias{getTransformations}
\title{Transformations}
\description{
  Predefined transformations (mappings) which can be used with
  \code{\link{tm_map}}.
}
\usage{
getTransformations()
}
\value{
  A character vector with transformations provided by package \pkg{tm}.
}
\seealso{
  \code{\link{removeNumbers}}, \code{\link{removePunctuation}},
  \code{\link{removeWords}}, \code{\link{stemDocument}}, and
  \code{\link{stripWhitespace}}.

  \code{\link{content_transformer}} to create custom transformations.
}  
\examples{
getTransformations()
}
