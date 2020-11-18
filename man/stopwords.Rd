\name{stopwords}
\alias{stopwords}
\title{Stopwords}
\description{
  Return various kinds of stopwords with support for different
  languages.
}
\usage{
stopwords(kind = "en")
}
\arguments{
  \item{kind}{A character string identifying the desired stopword list.}
}
\details{
  Available stopword lists are:
  \describe{
    \item{\code{catalan}}{Catalan stopwords (obtained from
      \url{http://latel.upf.edu/morgana/altres/pub/ca_stop.htm}),}
    \item{\code{romanian}}{Romanian stopwords (extracted from
      \url{http://snowball.tartarus.org/otherapps/romanian/romanian1.tgz}),}
    \item{\code{SMART}}{English stopwords from the SMART information
      retrieval system (as documented in Appendix 11 of
      \url{https://jmlr.csail.mit.edu/papers/volume5/lewis04a/})
      (which coincides with the stopword list used by the MC toolkit
      (\url{https://www.cs.utexas.edu/users/dml/software/mc/})),}
  }
  and a set of stopword lists from the Snowball stemmer project in different
  languages (obtained from
  \samp{http://svn.tartarus.org/snowball/trunk/website/algorithms/*/stop.txt}).
  Supported languages are \code{danish}, \code{dutch}, \code{english},
  \code{finnish}, \code{french}, \code{german}, \code{hungarian}, \code{italian},
  \code{norwegian}, \code{portuguese}, \code{russian}, \code{spanish}, and
  \code{swedish}. Language names are case sensitive. Alternatively, their
  \acronym{IETF} language tags may be used.
%
%  Earlier \pkg{tm} versions (before 2013-06-14) used merged stopword lists from
%  \url{http://www.ranks.nl/resources/stopwords.html} and the Snowball stemmer
%  project.
}
\value{A character vector containing the requested stopwords. An error
  is raised if no stopwords are available for the requested
  \code{kind}.}
\examples{
stopwords("en")
stopwords("SMART")
stopwords("german")
}
\keyword{file}
