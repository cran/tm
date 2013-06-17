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
  \item{kind}{A \code{character} identifying the desired stopword list.}
}
\details{
  Available stopword lists are:
  \describe{
    \item{\code{SMART}}{English stopwords from the SMART information
      retrieval system (obtained from
      \url{http://jmlr.csail.mit.edu/papers/volume5/lewis04a/a11-smart-stop-list/english.stop})
      (which coincides with the stopword list used by the MC toolkit
      (\url{http://www.cs.utexas.edu/users/dml/software/mc/})),}
    \item{\code{catalan}}{Catalan stopwords (obtained from
      \url{http://latel.upf.edu/morgana/altres/pub/ca_stop.htm}), and}
  }
  and a set of stopword lists from the Snowball stemmer project in different
  languages (obtained from
  \url{http://svn.tartarus.org/snowball/trunk/website/algorithms/*/stop.txt}).
  Supported languages are \code{danish}, \code{dutch}, \code{english},
  \code{finnish}, \code{french}, \code{german}, \code{hungarian}, \code{italian},
  \code{norwegian}, \code{portuguese}, \code{russian}, \code{spanish}, and
  \code{swedish}. Language names are case sensitive. Alternatively, their
  \acronym{IETF} language tags may be used.
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
