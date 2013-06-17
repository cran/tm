\name{weightSMART}
\alias{weightSMART}
\title{SMART Weightings}
\encoding{UTF-8}
\description{
  Weight a term-document matrix according to a combination of weights
  specified in SMART notation.
}
\usage{
weightSMART(m, spec = "nnn", control = list())
}
\arguments{
  \item{m}{A \code{\link{TermDocumentMatrix}} in term frequency format.}
  \item{spec}{a character string consisting of three characters. The first letter
    specifies a term frequency schema, the second a document frequency
    schema, and the third a normalization schema. See \bold{Details} for
    available built-in schemata.}
  \item{control}{a list of control parameters.  See \bold{Details}.}
}
\details{
  Formally this function is of class \code{WeightingFunction} with the
  additional attributes \code{Name} and \code{Acronym}.

  The first letter of \code{spec} specifies a weighting schema for term
  frequencies of \code{m}:

  \describe{
    \item{"n"}{(natural) \eqn{\mathit{tf}_{i,j}} counts the number of occurrences
      \eqn{n_{i,j}} of a term \eqn{t_i} in a document \eqn{d_j}. The
      input term-document matrix \code{m} is assumed to be in this
      standard term frequency format already.}
    \item{"l"}{(logarithm) is defined as \eqn{1 + \log(\mathit{tf}_{i,j})}.}
    \item{"a"}{(augmented) is defined as \eqn{0.5 +
	\frac{0.5 * \mathit{tf}_{i,j}}{\max_i(\mathit{tf}_{i,j})}}.}
    \item{"b"}{(boolean) is defined as 1 if \eqn{\mathit{tf}_{i,j} > 0} and 0 otherwise.}
    \item{"L"}{(log average) is defined as \eqn{\frac{1 +
    \log(\mathit{tf}_{i,j})}{1+\log(\mathrm{ave}_{i\in j}(\mathit{tf}_{i,j}))}}.}
  }

  The second letter of \code{spec} specifies a weighting schema of
  document frequencies for \code{m}:

  \describe{
    \item{"n"}{(no) is defined as 1.}
    \item{"t"}{(idf) is defined as \eqn{\log \frac{N}{\mathit{df}_t}} where
    \eqn{\mathit{df}_t} denotes how often term \eqn{t} occurs in all
      documents.}
    \item{"p"}{(prob idf) is defined as \eqn{\max(0, \log(\frac{N - \mathit{df}_t}{\mathit{df}_t}))}.}
  }

  The third letter of \code{spec} specifies a schema for normalization
  of \code{m}:

  \describe{
    \item{"n"}{(none) is defined as 1.}
    \item{"c"}{(cosine) is defined as \eqn{\sqrt{\mathrm{col\_sums}(m ^ 2)}}.}
    \item{"u"}{(pivoted unique) is defined as \eqn{\mathit{slope} *
      \sqrt{\mathrm{col\_sums}(m ^ 2)} + (1 - \mathit{slope}) *
      \mathit{pivot}} where both \code{slope} and \code{pivot} must be set
      via named tags in the \code{control} list.}
    \item{"b"}{(byte size) is defined as
      \eqn{\frac{1}{\mathit{CharLength}^\alpha}}. The parameter
      \eqn{\alpha} must be set via the named tag \code{alpha}
      in the \code{control} list.}
  }
  
  The final result is defined by multiplication of the chosen term
  frequency component with the chosen document frequency component with
  the chosen normalization component.
}
\value{
  The weighted matrix.
}
\references{
  Christopher D. Manning and Prabhakar Raghavan and Hinrich Schütze (2008).
  \emph{Introduction to Information Retrieval}.
  Cambridge University Press, ISBN 0521865719.
}
\examples{
data("crude")
TermDocumentMatrix(crude,
                   control = list(removePunctuation = TRUE,
                                  stopwords = TRUE,
                                  weighting = function(x)
                                  weightSMART(x, spec = "ntc")))
}
\author{Ingo Feinerer}
