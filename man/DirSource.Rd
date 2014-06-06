\name{DirSource}
\alias{DirSource}
\title{Directory Source}
\description{
  Create a directory source.
}
\usage{
DirSource(directory = ".",
          encoding = "",
          pattern = NULL,
          recursive = FALSE,
          ignore.case = FALSE,
          mode = "text")
}
\arguments{
  \item{directory}{A character vector of full path names; the default
    corresponds to the working directory \code{getwd()}.}
  \item{encoding}{a character string describing the current encoding. It is
    passed to \code{\link{iconv}} to convert the input to UTF-8.}
  \item{pattern}{an optional regular expression. Only file names which match
    the regular expression will be returned.}
  \item{recursive}{logical. Should the listing recurse into directories?}
  \item{ignore.case}{logical. Should pattern-matching be case-insensitive?}
  \item{mode}{a character string specifying if and how files should be read in.
    Available modes are:
    \describe{
      \item{\code{""}}{No read. In this case \code{\link{getElem}} and
        \code{\link{pGetElem}} only deliver \acronym{URI}s.}
      \item{\code{"binary"}}{Files are read in binary raw mode (via
        \code{\link{readBin}}).}
      \item{\code{"text"}}{Files are read as text (via
        \code{\link{readLines}}).}
    }
  }
}
\details{
  A \emph{directory source} acquires a list of files via \code{\link{dir}} and
  interprets each file as a document.
}
\value{
  An object inheriting from \code{DirSource}, \code{\link{SimpleSource}}, and
  \code{\link{Source}}.
}
\seealso{
  \code{\link{Source}} for basic information on the source infrastructure
  employed by package \pkg{tm}.

  \code{\link{Encoding}} and \code{\link{iconv}} on encodings.
}
\examples{
DirSource(system.file("texts", "txt", package = "tm"))
}
