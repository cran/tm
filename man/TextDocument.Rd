\name{TextDocument}
\alias{TextDocument}
\title{Text Documents}
\description{
  Representing and computing on text documents.
}
\details{
  \emph{Text documents} are documents containing (natural language) text. The
  \pkg{tm} package employs the infrastructure provided by package \pkg{NLP} and
  represents text documents via the virtual S3 class \code{TextDocument}.
  Actual S3 text document classes then extend the virtual base class (such as
  \code{\link{PlainTextDocument}}).

  All extension classes must provide an \code{\link{as.character}}
  method which extracts the natural language text in documents of the
  respective classes in a \dQuote{suitable} (not necessarily structured)
  form, as well as \code{\link{content}} and \code{\link{meta}} methods
  for accessing the (possibly raw) document content and metadata.
}
\seealso{
  \code{\link{PlainTextDocument}}, and
  \code{\link{XMLTextDocument}}
  for the text document classes provided by package \pkg{tm}.

  \code{\link[NLP]{TextDocument}} for text documents in package \pkg{NLP}.
}
