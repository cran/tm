# Author: Ingo Feinerer

.TermDocumentMatrix <-
function(i = integer(0), j = integer(0), v = numeric(0),
         nrow = 0, ncol = 0, dimnames = list(Terms = character(0), Docs = character(0)))
{
    structure(list(i = i, j = j, v = v, nrow = nrow, ncol = ncol, dimnames = dimnames),
              Weighting = c("term frequency", "tf"),
              class = c("TermDocumentMatrix", "simple_triplet_matrix"))
}

TermDocumentMatrix <- function(x, control = list()) UseMethod("TermDocumentMatrix", x)
TermDocumentMatrix.PCorpus <- TermDocumentMatrix.VCorpus <- function(x, control = list()) {
    weight <- control$weighting
    if (is.null(weight))
        weight <- weightTf

    lazyTmMap <- meta(x, tag = "lazyTmMap", type = "corpus")
    if (!is.null(lazyTmMap))
        .Call("copyCorpus", x, materialize(x))

    names(x) <- NULL

    tflist <- if (clusterAvailable())
        snow::parLapply(snow::getMPIcluster(), x, termFreq, control)
    else
        lapply(x, termFreq, control)
    tflist <- lapply(tflist, function(y) y[y > 0])

    v <- unlist(tflist)
    i <- names(v)
    allTerms <- sort(unique(if (is.null(control$dictionary)) i else control$dictionary))
    i <- match(i, allTerms)
    j <- rep(seq_along(x), sapply(tflist, length))

    tdm <- .TermDocumentMatrix(i = i, j = j, v = as.numeric(v), nrow = length(allTerms), ncol = length(x),
                               dimnames = list(Terms = allTerms, Docs = unlist(lapply(x, ID))))

    weight(tdm)
}

DocumentTermMatrix <- function(x, control = list())
    t(TermDocumentMatrix(x, control))

t.TermDocumentMatrix <- t.DocumentTermMatrix <- function(x) {
    m <- slam:::t.simple_triplet_matrix(x)
    attr(m, "Weighting") <- attr(x, "Weighting")
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

    # Punctuation removal
    removePunctuation <- control$removePunctuation
    if (isTRUE(removePunctuation))
        txt <- gsub("[[:punct:]]+", "", txt)
    else if (is.function(removePunctuation))
        txt <- removePunctuation(txt)
    else if (is.list(removePunctuation))
        txt <- do.call("removePunctuation", c(list(txt), removePunctuation))

    # Tokenize the corpus
    tokenize <- control$tokenize
    if(is.null(tokenize))
        tokenize <- scan_tokenizer
    else if(identical(tokenize, "MC"))
        tokenize <- MC_tokenizer
    txt <- tokenize(txt)

    # Number removal
    if (isTRUE(control$removeNumbers))
        txt <- gsub("[[:digit:]]+", "", txt)

    # Stopword filtering
    stopwords <- control$stopwords
    if (isTRUE(stopwords))
        txt <- txt[is.na(match(txt, stopwords(Language(doc))))]
    else if (is.character(stopwords))
        txt <- txt[is.na(match(txt, stopwords))]

    # Stemming
    stemming <- control$stemming
    if (isTRUE(stemming))
        txt <- stemDocument(txt, language = tm:::map_IETF(Language(doc)))
    else if (is.function(stemming))
        txt <- stemming(txt)

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
    cat(sprintf("\nNon-/sparse entries: %d/%.0f\n", length(x$v), prod(dim(x)) - length(x$v)))
    sparsity <- if (identical(prod(dim(x)), 0L)) 100 else round((1 - length(x$v)/prod(dim(x))) * 100)
    cat(sprintf("Sparsity           : %s%%\n", sparsity))
    cat("Maximal term length:", max(nchar(Terms(x), type = "chars"), 0), "\n")
    cat(sprintf("Weighting          : %s (%s)\n", attr(x, "Weighting")[1], attr(x, "Weighting")[2]))
}

inspect.TermDocumentMatrix <- inspect.DocumentTermMatrix <- function(x) {
    print(x)
    cat("\n")
    print(as.matrix(x))
}

`[.TermDocumentMatrix` <- `[.DocumentTermMatrix` <- function(x, i, j, ..., drop) {
    m <- slam:::`[.simple_triplet_matrix`(x, i, j, ...)
    attr(m, "Weighting") <- attr(x, "Weighting")
    class(m) <- if (inherits(x, "DocumentTermMatrix"))
        c("DocumentTermMatrix", "simple_triplet_matrix")
    else
        c("TermDocumentMatrix", "simple_triplet_matrix")
    m
}

nDocs <- function(x) if (inherits(x, "DocumentTermMatrix")) x$nrow else x$ncol
nTerms <- function(x) if (inherits(x, "DocumentTermMatrix")) x$ncol else x$nrow

Docs <- function(x) if (inherits(x, "DocumentTermMatrix")) x$dimnames[[1]] else x$dimnames[[2]]
Terms <- function(x) if (inherits(x, "DocumentTermMatrix")) x$dimnames[[2]] else x$dimnames[[1]]

c.TermDocumentMatrix <- function(x, ..., recursive = FALSE) {
    args <- list(...)

    if (identical(length(args), 0L))
        return(x)

    if (!all(unlist(lapply(args, inherits, "TermDocumentMatrix"))))
        stop("not all arguments are term-document matrices")

    m <- base::c(list(x), args)
    allTermsNonUnique <- unlist(lapply(m, function(x) Terms(x)[x$i]))
    allTerms <- unique(allTermsNonUnique)
    allDocs <- unlist(lapply(m, Docs))

    cs <- cumsum(lapply(m, nDocs))
    cs <- c(0, cs[-length(cs)])
    j <- lapply(m, "[[", "j")

    .TermDocumentMatrix(i = match(allTermsNonUnique, allTerms),
                        j = unlist(j) + rep.int(cs, sapply(j, length)),
                        v = unlist(lapply(m, "[[", "v")),
                        nrow = length(allTerms),
                        ncol = length(allDocs),
                        dimnames = list(Terms = allTerms, Docs = allDocs))
}

findFreqTerms <- function(x, lowfreq = 0, highfreq = Inf) {
    if (inherits(x, "DocumentTermMatrix")) x <- t(x)
    rs <- slam::row_sums(x)
    names(rs[rs >= lowfreq & rs <= highfreq])
}

findAssocs <- function(x, term, corlimit) UseMethod("findAssocs", x)
findAssocs.TermDocumentMatrix <- function(x, term, corlimit) findAssocs(t(x), term, corlimit)
findAssocs.DocumentTermMatrix <- function(x, term, corlimit) {
    suppressWarnings(x.cor <- cor(as.matrix(x)))
    findAssocs(x.cor, term, corlimit)
}
findAssocs.matrix <- function(x, term, corlimit)
    sort(round(x[term, which(x[term,] > corlimit)], 2), decreasing = TRUE)

removeSparseTerms <- function(x, sparse) {
    if ((sparse <= 0) || (sparse >= 1))
        stop("invalid sparse factor")
    else {
        m <- if (inherits(x, "DocumentTermMatrix")) t(x) else x
        t <- table(m$i) > m$ncol * (1 - sparse)
        termIndex <- as.numeric(names(t[t]))
        if (inherits(x, "DocumentTermMatrix")) x[, termIndex] else x[termIndex,]
    }
}
