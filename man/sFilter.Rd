\name{sFilter}
\alias{sFilter}
\title{Statement Filter}
\description{
  Filter meta data by user-defined statements.
}
\usage{
sFilter(x, s)
}
\arguments{
  \item{x}{A \code{Corpus}.}
  \item{s}{A statement of format \code{"tag1 == 'expr1' & tag2 == 'expr2' & ..."}.}
}
\details{
  The statement \code{s} models a simple query language. It consists of
  an expression as passed over to a data frame for subsetting. Tags in
  \code{s} represent meta data variables. Variables only available at
  document level are shifted up to the data frame if necessary. Note
  that the meta data tags for the slots \code{Author}, \code{DateTimeStamp},
  \code{Description}, \code{ID}, \code{Origin} and \code{Heading} are
  \code{author}, \code{datetimestamp}, \code{description},
  \code{id}, \code{origin} and \code{heading}, respectively, to
  avoid name conflicts.
}
\value{
  A logical vector to represent the subset of the \code{DMetaData}
  (extended for shifted up variables) data frame as specified by the
  statement.
}
\examples{
data("crude")
sFilter(crude, "id == '127' & heading == 'DIAMOND SHAMROCK (DIA) CUTS CRUDE PRICES'")
sFilter(crude, "Places == 'usa'")
}
\author{Ingo Feinerer}
