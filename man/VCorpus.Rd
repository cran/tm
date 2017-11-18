\name{VCorpus}
\alias{VCorpus}
\alias{as.VCorpus}
\title{Volatile Corpora}
\description{
  Create volatile corpora.
}
\usage{
VCorpus(x, readerControl = list(reader = reader(x), language = "en"))
as.VCorpus(x)
}
\arguments{
  \item{x}{For \code{VCorpus} a \code{\link{Source}} object, and for
    \code{as.VCorpus} an \R object.}
  \item{readerControl}{a named list of control parameters for reading in content
    from \code{x}.
    \describe{
      \item{\code{reader}}{a function capable of reading in and processing the
        format delivered by \code{x}.}
      \item{\code{language}}{a character giving the language (preferably as
        \acronym{IETF} language tags, see \link[NLP]{language} in
        package \pkg{NLP}).
        The default language is assumed to be English (\code{"en"}).}
    }
  }
}
\value{
  An object inheriting from \code{VCorpus} and \code{Corpus}.
}
\details{
  A \emph{volatile corpus} is fully kept in memory and thus all changes only
  affect the corresponding \R object.
}
\seealso{
  \code{\link{Corpus}} for basic information on the corpus infrastructure
  employed by package \pkg{tm}.

  \code{\link{PCorpus}} provides an implementation with permanent storage
  semantics.
}
\examples{
reut21578 <- system.file("texts", "crude", package = "tm")
VCorpus(DirSource(reut21578, mode = "binary"),
        list(reader = readReut21578XMLasPlain))
}
