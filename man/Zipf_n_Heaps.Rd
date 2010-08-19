\name{Zipf_n_Heaps}
\alias{Zipf_plot}
\alias{Heaps_plot}
\title{Explore Corpus Term Frequency Characteristics}
\description{
  Explore Zipf's law and Heaps' law, two empirical laws in linguistics
  describing commonly observed characteristics of term frequency
  distributions in corpora.
}
\usage{
Zipf_plot(x, type = "l", ...)
Heaps_plot(x, type = "l", ...)
}
\arguments{
  \item{x}{a document-term matrix or term-document matrix with
    unweighted term frequencies.}
  \item{type}{a character string indicating the type of plot to be
    drawn, see \code{\link{plot}}.}
  \item{...}{further graphical parameters to be used for plotting.}
}
\details{
  Zipf's law (e.g., \url{http://en.wikipedia.org/wiki/Zipf\%27s_law})
  states that given some corpus of natural language utterances, the
  frequency of any word is inversely proportional to its rank in the
  frequency table, or, more generally, that the pmf of the term
  frequencies is of the form \eqn{c k^{-\beta}}, where \eqn{k} is the
  rank of the term (taken from the most to the least frequent one).
  We can conveniently explore the degree to which the law holds by
  plotting the logarithm of the frequency against the logarithm of the
  rank, and inspecting the goodness of fit of a linear model.

  Heaps' law (e.g., \url{http://en.wikipedia.org/wiki/Heaps\%27_law})
  states that the vocabulary size \eqn{V} (i.e., the number of different
  terms employed) grows polynomially with the text size \eqn{T} (the
  total number of terms in the texts), so that \eqn{V = c T^\beta}.
  We can conveniently explore the degree to which the law holds by
  plotting \eqn{\log(V)} against \eqn{\log(T)}, and inspecting the
  goodness of fit of a linear model.
}
\value{
  The coefficients of the fitted linear model.  As a side effect, the
  corresponding plot is produced.
}
\examples{
data("acq")
m <- DocumentTermMatrix(acq)
Zipf_plot(m)
Heaps_plot(m)
}
