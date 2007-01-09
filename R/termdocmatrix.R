# Author: Ingo Feinerer
#
# Copyright notice:
# Parts of the preprocessing code were adapted from the \pkg{lsa} package. Special thanks to Fridolin Wild.

# Input matrix has to be in term-frequency format
weightMatrix <- function(m, weighting = "tf") {
    type <- match.arg(weighting,c("tf","tf-idf","bin","logical"))
    switch(type,
           "tf" = {
               wm <- m
           },
           "tf-idf" = {
               df <- colSums(((m > 0) * 1))
               wm <- m * log2(nrow(m) / df)
           },
           "bin" = {
               wm <- (m > 0) * 1
           },
           "logical" = {
               wm <- m > 0
           }
           )
    wm
}

setGeneric("TermDocMatrix", function(object, weighting = "tf", stemming = FALSE, language = "english", minWordLength = 3, minDocFreq = 1, stopwords = NULL) standardGeneric("TermDocMatrix"))
setMethod("TermDocMatrix",
          signature(object = "TextDocCol"),
          function(object, weighting = "tf", stemming = FALSE, language = "english",
                   minWordLength = 3, minDocFreq = 1, stopwords = NULL) {
              tvlist <- lapply(object, textvector, stemming, language, minWordLength, minDocFreq, stopwords)
              tm <- as.matrix(xtabs(Freq ~ ., data = do.call("rbind", tvlist)))
              class(tm) <- "matrix"
              tm <- weightMatrix(tm, weighting)

              new("TermDocMatrix", .Data = tm, Weighting = weighting)
          })

textvector <- function(doc, stemming = FALSE, language = "english", minWordLength = 3, minDocFreq = 1, stopwords = NULL) {
    txt <- gsub( "\\.|:|\\(|\\)|\\[|\\]|\\{|\\}|,|;|\\?|-|\\!|\"|\'|`|\\^|=|-|/", " ", doc)
    txt <- gsub("[[:space:]]+", " ", txt)
    txt <- tolower(txt)
    txt <- unlist(strsplit(txt, " ", fixed = TRUE))

    # stopword filtering?
    if (!is.null(stopwords)) txt = txt[!txt %in% stopwords]

    # tabulate
    tab <- sort(table(txt), decreasing = TRUE)

    # with threshold minDocFreq
    tab <- tab[tab >= minDocFreq]

    # wordLength filtering?
    tab <- tab[nchar(names(tab), type="chars") >= minWordLength]

    # Is the vector empty?
    if (is.null(names(tab))) {
        terms <- ""
        Freq <- 0
    }
    else {
        # stemming?
        if (stemming) {
            require("Rstem")
            names(tab) <- Rstem::wordStem(names(tab), language = language)
        }
        terms <- names(tab)
        Freq <- tab
    }

    data.frame(docs = ID(doc), terms, Freq, row.names = NULL)
}

setGeneric("findFreqTerms", function(object, lowfreq, highfreq) standardGeneric("findFreqTerms"))
setMethod("findFreqTerms",
          signature(object = "TermDocMatrix", lowfreq = "numeric", highfreq = "numeric"),
          function(object, lowfreq, highfreq) {
              unique(rownames(which(t(object) >= lowfreq & t(object) <= highfreq, arr.ind = TRUE)))
          })

setGeneric("findAssocs", function(object, term, corlimit) standardGeneric("findAssocs"))
setMethod("findAssocs",
          signature(object = "TermDocMatrix", term = "character"),
          function(object, term, corlimit) {
              suppressWarnings(object.cor <- cor(object))
              sort(round(object.cor[term, which(object.cor[term,] > corlimit)], 2), decreasing = TRUE)
          })
setMethod("findAssocs",
          signature(object = "matrix", term = "character"),
          function(object, term, corlimit) {
              sort(round(object[term, which(object[term,] > corlimit)], 2), decreasing = TRUE)
          })
