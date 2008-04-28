# Author: Ingo Feinerer

TermDocMatrix <- function(object, control = list()) {
    .Defunct("DocumentTermMatrix", package = "tm")
}

TermDocumentMatrix <- function(object, control = list()) {
    weight <- control$weighting
    if (is.null(weight))
        weight <- weightTf

    tflist <- if (clusterAvailable())
        snow::parLapply(snow::getMPIcluster(), object, termFreq, control)
    else
        lapply(object, termFreq, control)
    tflist <- lapply(tflist, function(x) x[x > 0])

    v <- unlist(tflist)
    i <- names(v)
    v <- as.numeric(v)
    allTerms <- sort(unique(i))
    i <- match(i, allTerms)
    j <- rep(seq_along(object), sapply(tflist, length))
    rm(tflist)

    tdm <- structure(list(i = i, j = j, v = v, nrow = length(allTerms), ncol = length(object),
                          dimnames = list(Terms = allTerms, Docs = sapply(object, ID)),
                          Weighting = c(weight@Name, weight@Acronym)),
                     class = c("TermDocumentMatrix", "simple_triplet_matrix"))
    weight(tdm)
}

DocumentTermMatrix <- function(object, control = list())
    t(TermDocumentMatrix(object, control))

t.TermDocumentMatrix <- t.DocumentTermMatrix <- function(x) {
    m <- t.simple_triplet_matrix(x)
    m$Weighting <- x$Weighting
    class(m) <- if (inherits(x, "DocumentTermMatrix"))
        c("TermDocumentMatrix", "simple_triplet_matrix")
    else
        c("DocumentTermMatrix", "simple_triplet_matrix")
    m
}

termFreq <- function(doc, control = list()) {
    txt <- Content(doc)

    # Conversion to lower characters
    tolower <- control$tolower
    if (is.null(tolower) || tolower)
        txt <- tolower(txt)

    # Tokenize the corpus
    tokenize <- control$tokenize
    if (is.null(tokenize))
        tokenize <- RWeka::AlphabeticTokenizer
    txt <- tokenize(txt)

    # Number removal
    if (isTRUE(control$removeNumbers))
        txt <- gsub("[[:digit:]]+", "", txt)

    # Stemming
    stemming <- control$stemming
    if (isTRUE(stemming))
        stemming <- function(x) Snowball::SnowballStemmer(x, RWeka::Weka_control(S = resolveISOCode(Language(doc))))
    if (is.function(stemming))
        txt <- stemming(txt)

    # Stopword filtering
    stopwords <- control$stopwords
    if (isTRUE(stopwords))
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

print.TermDocumentMatrix <- print.DocumentTermMatrix <- function(x, ...) {
    format <- c("term", "document")
    if (inherits(x, "DocumentTermMatrix")) format <- rev(format)
    cat(sprintf("A %s-%s matrix (%d %ss, %d %ss)\n",
                format[1], format[2], nrow(x), format[1], ncol(x), format[2]))
    cat(sprintf("\nNon-/sparse entries: %d/%d\n", length(x$v), prod(dim(x)) - length(x$v)))
    cat(sprintf("Sparsity           : %d%%\n", round((1 - length(x$v)/prod(dim(x))) * 100)))
    cat("Maximal term length:", max(nchar(Terms(x), type = "chars")), "\n")
    cat(sprintf("Weighting          : %s (%s)\n", x$Weighting[1], x$Weighting[2]))
}

inspect.TermDocumentMatrix <- inspect.DocumentTermMatrix <- function(x) {
    print(x)
    cat("\n")
    print(as.matrix(x))
}

`[.TermDocumentMatrix` <- `[.DocumentTermMatrix` <- function(x, i, j, ..., drop) {
    m <- `[.simple_triplet_matrix`(x, i, j, ...)
    m$Weighting <- x$Weighting
    class(m) <- if (inherits(x, "DocumentTermMatrix"))
        c("DocumentTermMatrix", "simple_triplet_matrix")
    else
        c("TermDocumentMatrix", "simple_triplet_matrix")
    m
}

dim.TermDocumentMatrix <- dim.DocumentTermMatrix <- function(x) c(x$nrow, x$ncol)
ncol.TermDocumentMatrix <- ncol.DocumentTermMatrix <- function(x) x$ncol
nrow.TermDocumentMatrix <- nrow.DocumentTermMatrix <- function(x) x$nrow

dimnames.TermDocumentMatrix <- dimnames.DocumentTermMatrix <- function(x) x$dimnames
colnames.TermDocumentMatrix <- colnames.DocumentTermMatrix <- function(x) x$dimnames[[2]]
rownames.TermDocumentMatrix <- rownames.DocumentTermMatrix <- function(x) x$dimnames[[1]]

nDocs <- function(x) if (inherits(x, "DocumentTermMatrix")) x$nrow else x$ncol
nTerms <- function(x) if (inherits(x, "DocumentTermMatrix")) x$ncol else x$nrow

Docs <- function(x) if (inherits(x, "DocumentTermMatrix")) x$dimnames[[1]] else x$dimnames[[2]]
Terms <- function(x) if (inherits(x, "DocumentTermMatrix")) x$dimnames[[2]] else x$dimnames[[1]]

findFreqTerms <- function(object, lowfreq = 0, highfreq = Inf) {
    if (inherits(object, "DocumentTermMatrix")) object <- t(object)
    Terms(object)[unique(object$i[object$v >= lowfreq & object$v <= highfreq])]
}

findAssocs <- function(x, term, corlimit) UseMethod("findAssocs", x)
findAssocs.TermDocumentMatrix <- function(x, term, corlimit) findAssocs(t(x), term, corlimit)
findAssocs.DocumentTermMatrix <- function(x, term, corlimit) {
    suppressWarnings(x.cor <- cor(as.matrix(x)))
    findAssocs(x.cor, term, corlimit)
}
findAssocs.matrix <- function(x, term, corlimit)
    sort(round(x[term, which(x[term,] > corlimit)], 2), decreasing = TRUE)

removeSparseTerms <- function(object, sparse) {
    if ((sparse <= 0) || (sparse >= 1))
        stop("invalid sparse factor")
    else {
        m <- if (inherits(object, "DocumentTermMatrix")) t(object) else object
        t <- table(m$i) > m$ncol * (1 - sparse)
        termIndex <- as.numeric(names(t[t]))
        if (inherits(object, "DocumentTermMatrix")) object[, termIndex] else object[termIndex,]
    }
}
