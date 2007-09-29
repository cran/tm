# Author: Ingo Feinerer
# Transformations

setGeneric("removeMultipart",
           function(object, ...) standardGeneric("removeMultipart"))
setMethod("removeMultipart",
          signature(object = "PlainTextDocument"),
          function(object, ...) {
              c <- Corpus(object)

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

              Corpus(object) <- if (length(r) == 0) c else r
              return(object)
          })

setGeneric("removeCitation",
           function(object, ...) standardGeneric("removeCitation"))
# Remove e-mail citations beginning with >
setMethod("removeCitation",
          signature(object = "PlainTextDocument"),
          function(object, ...) {
              citations <- grep("^[[:blank:]]*>", Corpus(object))
              if (length(citations) > 0)
                  Corpus(object) <- Corpus(object)[-citations]
              return(object)
          })

setGeneric("removePunctuation", function(object, ...) standardGeneric("removePunctuation"))
setMethod("removePunctuation",
          signature(object = "PlainTextDocument"),
          function(object, ...) {
              Corpus(object) <- gsub("[[:punct:]]+", "", Corpus(object))
              return(object)
          })

setGeneric("removeSignature",
           function(object, ...) standardGeneric("removeSignature"))
setMethod("removeSignature",
          signature(object = "PlainTextDocument"),
          function(object, marks = character(0), ...) {
              c <- Corpus(object)

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

              Corpus(object) <- c
              return(object)
          })

setGeneric("removeWords", function(object, stopwords, ...) standardGeneric("removeWords"))
setMethod("removeWords",
          signature(object = "PlainTextDocument", stopwords = "character"),
          function(object, stopwords, ...) {
              splittedCorpus <- unlist(strsplit(object, " ", fixed = TRUE))
              noStopwordsCorpus <- splittedCorpus[!splittedCorpus %in% stopwords]
              Corpus(object) <- paste(noStopwordsCorpus, collapse = " ")
              return(object)
          })

setGeneric("replaceWords", function(object, words, by, ...) standardGeneric("replaceWords"))
setMethod("replaceWords",
          signature(object = "PlainTextDocument", words = "character", by = "character"),
          function(object, words, by, ...) {
              pattern <- paste(words, collapse = "|")
              Corpus(object) <- gsub(pattern, by, Corpus(object))
              return(object)
          })

setGeneric("stemDoc", function(object, language = "english", ...) standardGeneric("stemDoc"))
setMethod("stemDoc",
          signature(object = "PlainTextDocument"),
          function(object, language = "english", ...) {
              splittedCorpus <- unlist(strsplit(object, " ", fixed = TRUE))
              stemmedCorpus <- if (require("Rstem", quietly = TRUE))
                  Rstem::wordStem(splittedCorpus, language)
              else
                  SnowballStemmer(splittedCorpus, Weka_control(S = language))
              Corpus(object) <- paste(stemmedCorpus, collapse = " ")
              return(object)
          })

setGeneric("stripWhitespace", function(object, ...) standardGeneric("stripWhitespace"))
setMethod("stripWhitespace",
          signature(object = "PlainTextDocument"),
          function(object, ...) {
              Corpus(object) <- gsub("[[:space:]]+", " ", object)
              return(object)
          })

setGeneric("tmTolower", function(object, ...) standardGeneric("tmTolower"))
setMethod("tmTolower",
          signature(object = "PlainTextDocument"),
          function(object, ...) {
              Corpus(object) <- tolower(object)
              return(object)
          })
