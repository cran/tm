# Author: Ingo Feinerer

# Input matrix has to be in term-frequency format
weightMatrix <- function(m, weighting = "tf") {
    type <- match.arg(weighting, c("tf", "tf-idf", "bin", "logical"))
    switch(type,
           "tf" = {
               return(m)
           },
           "tf-idf" = {
               df <- colSums(as(m > 0, "dgCMatrix"))
               return(t(t(m) * log2(nrow(m) / df)))
           },
           "bin" = {
               return(as(m > 0, "dgCMatrix"))
           },
           "logical" = {
               return(m > 0)
           })
}

setGeneric("TermDocMatrix",
           function(object, weighting = "tf", stemming = FALSE, minWordLength = 3,
                    minDocFreq = 1, stopwords = NULL, dictionary = NULL) standardGeneric("TermDocMatrix"))
# Kudos to Christian Buchta for significantly improving TermDocMatrix's efficiency
setMethod("TermDocMatrix",
          signature(object = "TextDocCol"),
          function(object, weighting = "tf", stemming = FALSE, minWordLength = 3,
                   minDocFreq = 1, stopwords = NULL, dictionary = NULL) {

              tvlist <- lapply(object, textvector, stemming, minWordLength, minDocFreq, stopwords, dictionary)
              terms <- lapply(tvlist, "[[", "terms")
              allTerms <- unique(unlist(terms, use.names = FALSE))

              i <- lapply(terms, match, allTerms)
              rm(terms)
              p <- cumsum(sapply(i, length))
              i <- unlist(i) - 1L

              x <- lapply(tvlist, "[[", "freqs")
              rm(tvlist)
              x <- as.numeric(unlist(x, use.names = FALSE))

              tdm <- new("dgCMatrix", p = c(0L, p), i = i, x = x,
                         Dim = c(length(allTerms), length(p)),
                         Dimnames = list(Terms = allTerms, Docs = sapply(object, ID)))
              tdm <- weightMatrix(t(tdm), weighting)

              new("TermDocMatrix", Data = tdm, Weighting = weighting)
          })

# Parts of this preprocessing code were adapted from the \pkg{lsa} package. Special thanks to Fridolin Wild.
textvector <- function(doc, stemming = FALSE, minWordLength = 3, minDocFreq = 1,
                       stopwords = NULL, dictionary = NULL) {
    txt <- gsub("[^[:alnum:]]+", " ", doc)
    txt <- tolower(txt)
    txt <- unlist(strsplit(txt, " ", fixed = TRUE))

    # stemming
    if (stemming) {
        txt <- if (require("Rstem", quietly = TRUE))
            Rstem::wordStem(txt, language = resolveISOCode(Language(doc)))
        else
            SnowballStemmer(txt, Weka_control(S = resolveISOCode(Language(doc))))
    }

    # stopword filtering?
    if (is.logical(stopwords) && stopwords)
        txt <- txt[!txt %in% stopwords(Language(doc))]
    else if (!is.logical(stopwords) && !is.null(stopwords))
        txt <- txt[!txt %in% stopwords]

    # if dictionary is set tabulate against it
    tab <-  if (is.null(dictionary))
        table(txt)
    else
        table(factor(txt, levels = dictionary))

    # with threshold minDocFreq
    tab <- tab[tab >= minDocFreq]

    # wordLength filtering?
    tab <- tab[nchar(names(tab), type = "chars") >= minWordLength]

    # Is the vector empty?
    if (length(names(tab)) <= 0) {
        terms <- ""
        freqs <- 0
    }
    else {
        terms <- names(tab)
        freqs <- tab
    }

    data.frame(docs = ID(doc), terms, freqs, row.names = NULL, stringsAsFactors = FALSE)
}

setMethod("dim",
          signature(x = "TermDocMatrix"),
          function(x) {
              dim(Data(x))
          })

setGeneric("findFreqTerms", function(object, lowfreq, highfreq) standardGeneric("findFreqTerms"))
setMethod("findFreqTerms",
          signature(object = "TermDocMatrix", lowfreq = "numeric", highfreq = "numeric"),
          function(object, lowfreq, highfreq) {
              m <- as(Data(object), "TsparseMatrix")
              colnames(m)[unique(m@j[m@x >= lowfreq & m@x <= highfreq]) + 1]
          })

setGeneric("findAssocs", function(object, term, corlimit) standardGeneric("findAssocs"))
setMethod("findAssocs",
          signature(object = "TermDocMatrix", term = "character"),
          function(object, term, corlimit) {
              object <- as(Data(object), "matrix")
              suppressWarnings(object.cor <- cor(object))
              sort(round(object.cor[term, which(object.cor[term,] > corlimit)], 2), decreasing = TRUE)
          })
setMethod("findAssocs",
          signature(object = "matrix", term = "character"),
          function(object, term, corlimit) {
              sort(round(object[term, which(object[term,] > corlimit)], 2), decreasing = TRUE)
          })

setGeneric("removeSparseTerms", function(object, sparse) standardGeneric("removeSparseTerms"))
setMethod("removeSparseTerms",
          signature(object = "TermDocMatrix", sparse = "numeric"),
          function(object, sparse) {
              if ((sparse <= 0) || (sparse >= 1))
                  warning("invalid sparse factor")
              else {
                  m <- as(Data(object), "TsparseMatrix")
                  t <- table(m@j + 1) > nrow(m) * (1 - sparse)
                  Data(object) <- m[, as.numeric(names(t[t]))]
              }
              return(object)
          })

setGeneric("createDictionary", function(object) standardGeneric("createDictionary"))
setMethod("createDictionary",
          signature(object = "TermDocMatrix"),
          function(object) {
              Dictionary(colnames(Data(object)))
          })
