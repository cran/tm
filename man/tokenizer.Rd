\name{tokenizer}
\alias{Boost_tokenizer}
\alias{MC_tokenizer}
\alias{scan_tokenizer}
\title{Tokenizers}
\description{Tokenize a document or character vector.}
\usage{
Boost_tokenizer(x)
MC_tokenizer(x)
scan_tokenizer(x)
}
\arguments{
  \item{x}{A character vector, or an object that can be coerced to character by
  \code{as.character}.}
}
\value{
  A character vector consisting of tokens obtained by tokenization of \code{x}.
}
\details{
  The quality and correctness of a tokenization algorithm highly depends
  on the context and application scenario. Relevant factors are the
  language of the underlying text and the notions of whitespace (which
  can vary with the used encoding and the language) and punctuation
  marks. Consequently, for superior results you probably need a custom
  tokenization function.

  \describe{
    \item{Boost_tokenizer}{Uses the Boost (\url{https://www.boost.org})
      Tokenizer (via \pkg{Rcpp}).}
    \item{MC_tokenizer}{Implements the functionality of the tokenizer in the
      MC toolkit (\url{https://www.cs.utexas.edu/users/dml/software/mc/}).}
    \item{scan_tokenizer}{Simulates \code{scan(\dots, what = "character")}.}
  }
}
\seealso{
  \code{\link{getTokenizers}} to list tokenizers provided by package \pkg{tm}.

  \code{\link[NLP]{Regexp_Tokenizer}} for tokenizers using regular expressions
    provided by package \pkg{NLP}.

  \code{\link[tau]{tokenize}} for a simple regular expression based tokenizer
    provided by package \pkg{tau}.

  \code{\link[tokenizers]{tokenizers}} for a collection of tokenizers provided
    by package \pkg{tokenizers}.
}
\examples{
data("crude")
Boost_tokenizer(crude[[1]])
MC_tokenizer(crude[[1]])
scan_tokenizer(crude[[1]])
strsplit_space_tokenizer <- function(x)
    unlist(strsplit(as.character(x), "[[:space:]]+"))
strsplit_space_tokenizer(crude[[1]])
}
