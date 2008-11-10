# Author: Ingo Feinerer

setGeneric("TermDocMatrix",
           function(object, control = list()) standardGeneric("TermDocMatrix"))
# Kudos to Christian Buchta for significantly improving TermDocMatrix's efficiency
setMethod("TermDocMatrix",
          signature(object = "Corpus"),
          function(object, control = list()) {

              weight <- control$weighting
              if (is.null(weight))
                  weight <- weightTf

              tflist <- if (clusterAvailable())
                  snow::parLapply(snow::getMPIcluster(), object, termFreq, control)
              else
                  lapply(object, termFreq, control)
              allTerms <- unique(unlist(lapply(tflist, names), use.names = FALSE))

              i <- lapply(tflist, function(x) match(names(x), allTerms)[x > 0])
              p <- cumsum(sapply(i, length))
              i <- unlist(i) - 1L

              x <- as.numeric(unlist(tflist, use.names = FALSE))
              rm(tflist)

              tdm <- new("dgCMatrix", p = c(0L, p), i = i, x = x[x > 0],
                         Dim = c(length(allTerms), length(p)))
              tdm <- weight(t(tdm))
              tdm@Dimnames <- list(Docs = sapply(object, ID), Terms = allTerms)

              new("TermDocMatrix", Data = tdm, Weighting = c(weight@Name, weight@Acronym))
          })

termFreq <- function(doc, control = list()) {
    txt <- Content(doc)

    # Conversion to lower characters
    tolower <- control$tolower
    if (is.null(tolower) || tolower)
        txt <- tolower(txt)

    # Tokenize the corpus
    tokenize <- control$tokenize
    if (is.null(tokenize))
        tokenize <- function(x) unlist(strsplit(gsub("[^[:alnum:]]+", " ", x), " ", fixed = TRUE))
    txt <- tokenize(txt)

    removeNumbers <- control$removeNumbers
    if (is.logical(removeNumbers) && removeNumbers)
        txt <- gsub("[[:digit:]]+", "", txt)

    # Stemming
    stemming <- control$stemming
    if (is.logical(stemming) && stemming) {
        txt <- if (suppressWarnings(require("Rstem", quietly = TRUE)))
            Rstem::wordStem(txt, language = resolveISOCode(Language(doc)))
        else
            SnowballStemmer(txt, Weka_control(S = resolveISOCode(Language(doc))))
    }

    # Stopword filtering
    stopwords <- control$stopwords
    if (is.logical(stopwords) && stopwords)
        txt <- txt[!txt %in% stopwords(Language(doc))]
    else if (is.character(stopwords))
        txt <- txt[!txt %in% stopwords]

    # Check if the document content is NULL
    if (is.null(txt))
        return(structure(integer(0), names = character(0)))

    # If dictionary is set tabulate against it
    dictionary <- control$dictionary
    tab <-  if (is.null(dictionary))
        table(txt)
    else
        table(factor(txt, levels = dictionary))

    # Ensure minimum document frequency threshold
    minDocFreq <- control$minDocFreq
    if (!is.null(minDocFreq))
        tab <- tab[tab >= minDocFreq]

    # Filter out too short terms
    minWordLength <- control$minWordLength
    if (is.null(minWordLength))
        minWordLength <- 3
    tab <- tab[nchar(names(tab), type = "chars") >= minWordLength]

    # Return named integer
    structure(as.integer(tab), names = names(tab))
}

setMethod("[",
          signature(x = "TermDocMatrix", i = "ANY", j = "ANY", drop = "ANY"),
          function(x, i, j, ..., drop) {
              # Unfortunately Data(x)[i, j, ..., drop] alone does not work when j is missing
              if (missing(i)) return(Data(x))
              if (missing(j)) return(Data(x)[i, j = seq_len(ncol(x)), ..., drop])
              Data(x)[i, j, ..., drop]
          })

setMethod("dim",
          signature(x = "TermDocMatrix"),
          function(x) {
              dim(Data(x))
          })

setMethod("ncol",
          signature(x = "TermDocMatrix"),
          function(x) {
              ncol(Data(x))
          })

setMethod("nrow",
          signature(x = "TermDocMatrix"),
          function(x) {
              nrow(Data(x))
          })

setMethod("dimnames",
          signature(x = "TermDocMatrix"),
          function(x) {
              dimnames(Data(x))
          })

setMethod("colnames",
          signature(x = "TermDocMatrix"),
          function(x, do.NULL = TRUE, prefix = "col") {
              colnames(Data(x), do.NULL, prefix)
          })

setMethod("rownames",
          signature(x = "TermDocMatrix"),
          function(x, do.NULL = TRUE, prefix = "row") {
              rownames(Data(x), do.NULL, prefix)
          })

setMethod("as.matrix",
          signature(x = "TermDocMatrix"),
          function(x) {
              as.matrix(Data(x))
          })

setGeneric("findFreqTerms", function(object, lowfreq = 0, highfreq = Inf) standardGeneric("findFreqTerms"))
setMethod("findFreqTerms",
          signature(object = "TermDocMatrix"),
          function(object, lowfreq = 0, highfreq = Inf) {
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
