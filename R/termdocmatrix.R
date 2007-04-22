# Author: Ingo Feinerer
#
# Copyright notice:
# Parts of the preprocessing code were adapted from the \pkg{lsa} package. Special thanks to Fridolin Wild.

# Input matrix has to be in term-frequency format
weightMatrix <- function(m, weighting = "tf") {
    type <- match.arg(weighting, c("tf", "tf-idf", "bin", "logical"))
    switch(type,
           "tf" = {
               return(m)
           },
           "tf-idf" = {
               df <- colSums(as(m > 0, "dgCMatrix"))
               return(m * log2(nrow(m) / df))
           },
           "bin" = {
               return(as(m > 0, "dgCMatrix"))
           },
           "logical" = {
               return(m > 0)
           })
}

setGeneric("TermDocMatrix", function(object, weighting = "tf", stemming = FALSE, minWordLength = 3, minDocFreq = 1, stopwords = NULL) standardGeneric("TermDocMatrix"))
setMethod("TermDocMatrix",
          signature(object = "TextDocCol"),
          function(object, weighting = "tf", stemming = FALSE,
                   minWordLength = 3, minDocFreq = 1, stopwords = NULL) {

              tvlist <- lapply(object, textvector, stemming, minWordLength, minDocFreq, stopwords)
              allTerms <- unique(unlist(lapply(tvlist, "[[", "terms")))

              tdm <- Matrix(0,
                            nrow = length(object),
                            ncol = length(allTerms),
                            dimnames = list(sapply(object, ID), allTerms))

              for(i in seq_along(object)) {
                  df <- tvlist[[i]]
                  j <- match(df$terms, allTerms)
                  tdm[i, j] <- df$freqs
              }

              tdm <- weightMatrix(tdm, weighting)

              new("TermDocMatrix", Data = tdm, Weighting = weighting)
          })

textvector <- function(doc, stemming = FALSE, minWordLength = 3, minDocFreq = 1, stopwords = NULL) {
    txt <- gsub("[^[:alnum:]]+", " ", doc)
    txt <- tolower(txt)
    txt <- unlist(strsplit(txt, " ", fixed = TRUE))

    # stemming
    if (stemming) {
        txt <- if (require("Rstem"))
            Rstem::wordStem(txt, language = resolveISOCode(Language(doc)))
        else
            SnowballStemmer(txt, Weka_control(S = resolveISOCode(Language(doc))))
    }

    # stopword filtering?
    if (is.logical(stopwords) && stopwords)
        txt <- txt[!txt %in% stopwords(Language(doc))]
    else if (!is.logical(stopwords) && !is.null(stopwords))
        txt <- txt[!txt %in% stopwords]

    # tabulate
    tab <- sort(table(txt), decreasing = TRUE)

    # with threshold minDocFreq
    tab <- tab[tab >= minDocFreq]

    # wordLength filtering?
    tab <- tab[nchar(names(tab), type = "chars") >= minWordLength]

    # Is the vector empty?
    if (is.null(names(tab))) {
        terms <- ""
        freqs <- 0
    }
    else {
        terms <- names(tab)
        freqs <- tab
    }

    data.frame(docs = ID(doc), terms, freqs, row.names = NULL, stringsAsFactors = FALSE)
}

setGeneric("findFreqTerms", function(object, lowfreq, highfreq) standardGeneric("findFreqTerms"))
setMethod("findFreqTerms",
          signature(object = "TermDocMatrix", lowfreq = "numeric", highfreq = "numeric"),
          function(object, lowfreq, highfreq) {
              object <- as(Data(object), "matrix")
              unique(rownames(which(t(object) >= lowfreq & t(object) <= highfreq, arr.ind = TRUE)))
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
