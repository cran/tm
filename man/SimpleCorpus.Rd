\name{SimpleCorpus}
\alias{SimpleCorpus}
\title{Simple Corpora}
\description{
  Create simple corpora.
}
\usage{
SimpleCorpus(x, control = list(language = "en"))
}
\arguments{
  \item{x}{a \code{\link{DataframeSource}}, \code{\link{DirSource}} or
    \code{\link{VectorSource}}.}
  \item{control}{a named list of control parameters.
    \describe{
      \item{\code{language}}{a character giving the language (preferably as
        \acronym{IETF} language tags, see \link[NLP]{language} in
        package \pkg{NLP}).
        The default language is assumed to be English (\code{"en"}).}
    }
  }
}
\value{
  An object inheriting from \code{SimpleCorpus} and \code{Corpus}.
}
\details{
  A \emph{simple corpus} is fully kept in memory. Compared to a \code{VCorpus},
  it is optimized for the most common usage scenario: importing plain texts from
  files in a directory or directly from a vector in \R, preprocessing and
  transforming the texts, and finally exporting them to a term-document matrix.
  It adheres to the \code{\link{Corpus}} \acronym{API}. However, it takes
  internally various shortcuts to boost performance and minimize memory
  pressure; consequently it operates only under the following contraints:
  \itemize{
    \item{only \code{DataframeSource}, \code{DirSource} and \code{VectorSource}
      are supported,}
    \item{no custom readers, i.e., each document is read in and stored as plain
      text (as a string, i.e., a character vector of length one),}
    \item{transformations applied via \code{\link{tm_map}} must be able to
      process character vectors and return character vectors (of the same
      length),}
    \item{no lazy transformations in \code{\link{tm_map}},}
    \item{no meta data for individual documents (i.e., no \code{"local"} in
      \code{\link{meta}}).}
  }
}
\seealso{
  \code{\link{Corpus}} for basic information on the corpus infrastructure
  employed by package \pkg{tm}.

  \code{\link{VCorpus}} provides an implementation with volatile storage
  semantics, and \code{\link{PCorpus}} provides an implementation with
  permanent storage semantics.
}
\examples{
txt <- system.file("texts", "txt", package = "tm")
(ovid <- SimpleCorpus(DirSource(txt, encoding = "UTF-8"),
                      control = list(language = "lat")))
}
