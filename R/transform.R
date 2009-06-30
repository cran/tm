# Author: Ingo Feinerer
# Transformations

tmReduce <- function(x, tmFuns, ...)
    Reduce(function(f, ...) f(...), tmFuns, x, right = TRUE)

getTransformations <- function() { c("asPlain",
    "removeCitation", "removeMultipart", "removeNumbers",
    "removePunctuation", "removeSignature", "removeWords",
    "replacePatterns", "stemDoc", "stripWhitespace", "tmTolower") }

setGeneric("removeMultipart",
           function(object, ...) standardGeneric("removeMultipart"))
setMethod("removeMultipart",
          signature(object = "PlainTextDocument"),
          function(object, ...) {
              c <- Content(object)

              # http://en.wikipedia.org/wiki/Multipart_message#Multipart_Messages
              # We are only interested in text/plain parts
              i <- grep("^Content-Type: text/plain", c)
              r <- character(0)
              k <- 2
              for (j in i) {
                  end <- if (k <= length(i)) i[k]-1 else length(c)
                  content <- c[j:end]
                  ## Find boundary (starting with "--")
                  # In most cases the boundary is just one line before the Content-Type header
                  start <- j - 1
                  while (j > 0) {
                      if (substr(c[j], 1, 2) == "--") {
                          start <- j
                          break
                      }
                      else
                          j <- j - 1
                  }
                  index <- grep(c[start], content)
                  index <- if (length(index) == 0) length(content) else (index[1] - 1)
                  content <- content[1:index]
                  # Now remove remaining headers
                  index <- grep("^$", content)
                  index <- if (length(index) == 0) 1 else (index[1] + 1)
                  r <- c(r, content[index:length(content)])
                  k <- k + 1
              }

              Content(object) <- if (length(r) == 0) c else r
              return(object)
          })

setGeneric("removeCitation",
           function(object, ...) standardGeneric("removeCitation"))
# Remove e-mail citations beginning with >
setMethod("removeCitation",
          signature(object = "PlainTextDocument"),
          function(object, ...) {
              citations <- grep("^[[:blank:]]*>", Content(object))
              if (length(citations) > 0)
                  Content(object) <- Content(object)[-citations]
              return(object)
          })

setGeneric("removeNumbers", function(object, ...) standardGeneric("removeNumbers"))
.removeNumbers <- function(object, ...) {
    Content(object) <- gsub("[[:digit:]]+", "", object)
    object
}
setMethod("removeNumbers", signature(object = "PlainTextDocument"), .removeNumbers)
#setMethod("removeNumbers", signature(object = "MinimalDocument"), .removeNumbers)

setGeneric("removePunctuation", function(object, ...) standardGeneric("removePunctuation"))
.removePunctuation <- function(object, ...) {
    Content(object) <- gsub("[[:punct:]]+", "", Content(object))
    object
}
setMethod("removePunctuation", signature(object = "PlainTextDocument"), .removePunctuation)
#setMethod("removePunctuation", signature(object = "MinimalDocument"), .removePunctuation)

setGeneric("removeSignature",
           function(object, ...) standardGeneric("removeSignature"))
setMethod("removeSignature",
          signature(object = "PlainTextDocument"),
          function(object, marks = character(0), ...) {
              c <- Content(object)

              # "---" is often added to Sourceforge mails
              # "___" and "***" are also common, i.e.,
              # marks <- c("^_{10,}", "^-{10,}", "^[*]{10,}")

              # "-- " is the official correct signature start mark
              marks <- c("^-- $", marks)

              signatureStart <- length(c) + 1
              for (m in marks)
                  signatureStart <- min(grep(m, c), signatureStart)

              if (signatureStart <= length(c))
                  c <- c[-(signatureStart:length(c))]

              Content(object) <- c
              return(object)
          })

setGeneric("removeWords", function(object, words, ...) standardGeneric("removeWords"))
.removeWords <- function(object, words, ...) {
    Content(object) <- gsub(paste("([[:blank:]]|^)",
                                  paste(words, collapse = "([[:blank:]]|$)|([[:blank:]]|^)"),
                                  "([[:blank:]]|$)", sep = ""),
                            " ",
                            # Add blank so that adjacent words can be matched
                            gsub("([[:blank:]])", "\\1 ", Content(object)))
    # Remove doubled blanks
    Content(object) <- gsub("([[:blank:]]) ", "\\1", Content(object))
    object
}
setMethod("removeWords", signature(object = "PlainTextDocument", words = "character"), .removeWords)
#setMethod("removeWords", signature(object = "MinimalDocument", words = "character"), .removeWords)

setGeneric("replacePatterns", function(object, patterns, by, ...) standardGeneric("replacePatterns"))
.replacePatterns <- function(object, patterns, by, ...) {
    Content(object) <- gsub(patterns, by, Content(object))
    object
}
setMethod("replacePatterns", signature(object = "PlainTextDocument", patterns = "character", by = "character"), .replacePatterns)
#setMethod("replacePatterns", signature(object = "MinimalDocument", patterns = "character", by = "character"), .replacePatterns)

setGeneric("stemDoc", function(object, language = "english", ...) standardGeneric("stemDoc"))
.stemDoc <- function(object, language = "english", ...) {
    stemLine <- function(x) Snowball::SnowballStemmer(x, RWeka::Weka_control(S = language))
    s <- sapply(object,
                function(x) paste(stemLine(unlist(strsplit(x, "[[:blank:]]"))), collapse = " "),
                USE.NAMES = FALSE)
    Content(object) <- if (is.character(s)) s else ""
    object
}
setMethod("stemDoc", signature(object = "PlainTextDocument"), .stemDoc)
#setMethod("stemDoc", signature(object = "MinimalDocument"), .stemDoc)

setGeneric("stripWhitespace", function(object, ...) standardGeneric("stripWhitespace"))
.stripWhitespace <- function(object, ...) {
    Content(object) <- gsub("[[:space:]]+", " ", object)
    object
}
setMethod("stripWhitespace", signature(object = "PlainTextDocument"), .stripWhitespace)
#setMethod("stripWhitespace", signature(object = "MinimalDocument"), .stripWhitespace)

setGeneric("tmTolower", function(object, ...) standardGeneric("tmTolower"))
.tmTolower <- function(object, ...) {
    Content(object) <- tolower(object)
    object
}
setMethod("tmTolower", signature(object = "PlainTextDocument"), .tmTolower)
#setMethod("tmTolower", signature(object = "MinimalDocument"), .tmTolower)
