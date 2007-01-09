\name{findAssocs-methods}
\docType{methods}
\alias{findAssocs}
\alias{findAssocs-methods}
\alias{findAssocs,TermDocMatrix,character-method}
\alias{findAssocs,matrix,character-method}
\title{Methods for Function findAssocs in Package `textmin'}
\description{
Methods for function \code{findAssocs} in Package `textmin'
}
\section{Methods}{
\describe{

\item{object = "TermDocMatrix", term = "character", corlimit}{finds those terms
  from \code{object} which correlate with \code{term} more than \code{corlimit}}

\item{object = "matrix", term = "character", corlimit}{finds those terms
  from the correlation matrix \code{object} which correlate with \code{term} more than \code{corlimit}}
}}
\keyword{methods}
