\name{ZipSource}
\alias{ZipSource}
\title{ZIP File Source}
\description{
  Create a ZIP file source.
}
\usage{
ZipSource(zipfile,
          pattern = NULL,
          recursive = FALSE,
          ignore.case = FALSE,
          mode = "text")
}
\arguments{
  \item{zipfile}{A character string with the full path name of a ZIP file.}
  \item{pattern}{an optional regular expression. Only file names in the ZIP
    file which match the regular expression will be returned.}
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
  A \emph{ZIP file source} extracts a compressed ZIP file via
  \code{\link{unzip}} and interprets each file as a document.
}
\value{
  An object inheriting from \code{ZipSource}, \code{\link{SimpleSource}}, and
  \code{\link{Source}}.
}
\seealso{
  \code{\link{Source}} for basic information on the source infrastructure
  employed by package \pkg{tm}.
}
\examples{
zipfile <- tempfile()
files <- Sys.glob(file.path(system.file("texts", "txt", package = "tm"), "*"))
zip(zipfile, files)
zipfile <- paste0(zipfile, ".zip")
Corpus(ZipSource(zipfile, recursive = TRUE))[[1]]
file.remove(zipfile)
}
