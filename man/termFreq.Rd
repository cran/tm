\name{termFreq}
\alias{termFreq}
\title{Term Frequency Vector}
\description{
  Generate a term frequency vector from a text document.
}
\usage{
termFreq(doc, control = list())
}
\arguments{
  \item{doc}{An object inheriting from \code{TextDocument}.}
  \item{control}{A list of control options which override default
    settings.

    First, following two options are processed.
    \describe{
      \item{\code{tolower}}{Either a logical value indicating whether
	characters should be translated to lower case or a custom function
	converting characters to lower case. Defaults to
	\code{\link{tolower}}.}
      \item{\code{tokenize}}{A function tokenizing documents into single
	tokens or a string matching one of the predefined tokenization
	functions:
	\describe{
	  \item{\code{scan}}{for \code{\link{scan_tokenizer}}, or}
	  \item{\code{MC}}{for \code{\link{MC_tokenizer}}.}
	}
	Defaults to \code{\link{scan_tokenizer}}.}
    }  
    Next, a set of options which are sensitive to the order of
    occurrence in the \code{control} list. Options are processed in the
    same order as specified. User-specified options have precedence over
    the default ordering so that first all user-specified options and
    then all remaining options (with the default settings and in the
    order as listed below) are processed.
    \describe{
      \item{\code{removePunctuation}}{A logical value indicating whether
	punctuation characters should be removed from
	\code{doc}, a custom function which performs punctuation
	removal, or a list of arguments for
	\code{\link{removePunctuation}}. Defaults to \code{FALSE}.}
      \item{\code{removeNumbers}}{A logical value indicating whether
	numbers should be removed from \code{doc} or a custom function
	for number removal. Defaults to \code{FALSE}.}
      \item{\code{stopwords}}{Either a Boolean value indicating stopword
	removal using default language specific stopword lists shipped
	with this package, a character vector holding custom
	stopwords, or a custom function for stopword removal. Defaults
	to \code{FALSE}.}
      \item{\code{stemming}}{Either a Boolean value indicating whether tokens
	should be stemmed or a custom stemming function. Defaults to
	\code{FALSE}.}
    }
    Finally, following options are processed in the given order.
    \describe{
      \item{\code{dictionary}}{A character vector to be tabulated
	against. No other terms will be listed in the result. Defaults
	to \code{NULL} which means that all terms in \code{doc} are
	listed.}
      \item{\code{bounds}}{A list with a tag \code{local} whose value
	must be an integer vector of length 2. Terms that appear less
	often in \code{doc} than the lower bound \code{bounds$local[1]}
	or more often than the upper bound \code{bounds$local[2]} are
	discarded. Defaults to \code{list(local = c(1,Inf))} (i.e., every
	token will be used).}
      \item{\code{wordLengths}}{An integer vector of length 2. Words
	shorter than the minimum word length \code{wordLengths[1]} or
	longer than the maximum word length \code{wordLengths[2]} are
	discarded. Defaults to \code{c(3, Inf)}, i.e., a minimum word
	length of 3 characters.}
    }
  }
}
\value{
  A named integer vector of class \code{term_frequency} with term
  frequencies as values and tokens as names.
}
\seealso{
  \code{\link{getTokenizers}}
}
\examples{
data("crude")
termFreq(crude[[14]])
strsplit_space_tokenizer <- function(x) unlist(strsplit(x, "[[:space:]]+"))
ctrl <- list(tokenize = strsplit_space_tokenizer,
             removePunctuation = list(preserve_intra_word_dashes = TRUE),
             stopwords = c("reuter", "that"),
             stemming = TRUE,
             wordLengths = c(4, Inf))
termFreq(crude[[14]], control = ctrl)
}
\keyword{math}
