\name{weightLogical}
\alias{weightLogical}
\title{Weight Logical}
\description{
  Weight a term-document matrix such that positive term frequencies map
  to Boolean \code{TRUE} values.
}
\usage{
weightLogical(m)
}
\arguments{
  \item{m}{A \code{dgCMatrix} in term frequency format.}
}
\details{
  Formally this function is a \code{WeightingFunction} with the additional
  slot \code{Name}.
}
\value{
  The weighted \code{dgCMatrix}.
}
\author{Ingo Feinerer}
\keyword{file}
