\name{convertMboxEml}
\alias{convertMboxEml}
\title{Convert E-Mails From mbox Format To eml Format}
\description{
  Convert e-mails from mbox (i.e., several mails in a single box) format
  to eml (i.e., every mail in a single file) format.
}
\usage{
convertMboxEml(mbox, EmlDir)
}
\arguments{
  \item{mbox}{a character describing the mbox location.}
  \item{EmlDir}{a character describing the output directory.}
}
\value{
  No explicit return value. As a side product the directory
  \code{EmlDir} contains the e-mails.
}
\author{Ingo Feinerer}
\examples{
mbox <- system.file("texts", "gmane.comp.lang.r.general.mbox", package = "tm")
\dontrun{convertMboxEml(mbox, "emldir/")}
}
\keyword{file}
