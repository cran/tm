\name{PCorpus}
\alias{PCorpus}
\title{Permanent Corpora}
\description{
  Create permanent corpora.
}
\usage{
PCorpus(x,
        readerControl = list(reader = reader(x), language = "en"),
        dbControl = list(dbName = "", dbType = "DB1"))
}
\arguments{
  \item{x}{A \code{\link{Source}} object.}
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
  \item{dbControl}{a named list of control parameters for the underlying
    database storage provided by package \pkg{filehash}.
    \describe{
      \item{\code{dbName}}{a character giving the filename for the database.}
      \item{\code{dbType}}{a character giving the database format (see
        \code{\link[filehash]{filehashOption}} for possible database formats).}
    }
  }
}
\value{
  An object inheriting from \code{PCorpus} and \code{Corpus}.
}
\details{
  A \emph{permanent corpus} stores documents outside of \R in a database. Since
  multiple \code{PCorpus} \R objects with the same underlying database can
  exist simultaneously in memory, changes in one get propagated to all
  corresponding objects (in contrast to the default \R semantics).
}
\seealso{
  \code{\link[tm]{Corpus}} for basic information on the corpus infrastructure
  employed by package \pkg{tm}.

  \code{\link{VCorpus}} provides an implementation with volatile storage
  semantics.
}
\examples{
txt <- system.file("texts", "txt", package = "tm")
\dontrun{
PCorpus(DirSource(txt),
        dbControl = list(dbName = "pcorpus.db", dbType = "DB1"))}
}
