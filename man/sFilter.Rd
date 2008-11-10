\name{sFilter}
\alias{sFilter}
\title{Statement Filter}
\description{
  Filter meta data by special statements.
}
\usage{
sFilter(object, s, ...)
}
\arguments{
  \item{object}{A \code{Corpus} to be the filter applied to.}
  \item{s}{A statement of format \code{"tag1 == 'expr1' & tag2 ==
      'expr2' & ..."}.}
  \item{...}{Arguments passed over by calling functions.}
}
\details{
  The statement \code{s} models a simple query language. It consists of
  an expression as passed over to a data frame for subsetting. Tags in
  \code{s} represent meta data variables. Variables only available at
  document level are shifted up to the data frame if necessary. Note
  that the meta data tags for the slots \code{Author}, \code{DateTimeStamp},
  \code{Description}, \code{ID}, \code{Origin} and \code{Heading} are
  \code{author}, \code{datetimestamp}, \code{description},
  \code{identifier}, \code{origin} and \code{heading}, respectively, to
  avoid name conflicts.
}
\value{
  A logical vector to represent the subset of the \code{DMetaData}
  (extended for shifted up variables) data frame as specified by the
  statement.
}
\seealso{
  Use \code{\link{getFilters}} to list available filter functions.
}
\examples{
## Load example dataset
data("crude")

## Statement Filter returning TRUE for document with ID 127 and heading DIAMOND ...
sFilter(crude, "identifier == '127' & heading == 'DIAMOND SHAMROCK (DIA) CUTS CRUDE PRICES'")
}
\author{Ingo Feinerer}
\keyword{file}
