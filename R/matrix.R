## Authors: Ingo Feinerer, Kurt Hornik

TermDocumentMatrix_classes <-
    c("TermDocumentMatrix", "simple_triplet_matrix")
DocumentTermMatrix_classes <-
    c("DocumentTermMatrix", "simple_triplet_matrix")

.TermDocumentMatrix <-
function(x, weighting)
{
    x <- as.simple_triplet_matrix(x)
    if(!is.null(dimnames(x)))
        names(dimnames(x)) <- c("Terms", "Docs")
    class(x) <- TermDocumentMatrix_classes
    ## <NOTE>
    ## Note that if weighting is a weight function, it already needs to
    ## know whether we have a term-document or document-term matrix.
    ##
    ## Ideally we would require weighting to be a WeightFunction object
    ## or a character string of length 2.  But then
    ##   dtm <- DocumentTermMatrix(crude,
    ##                             control = list(weighting =
    ##                                            function(x)
    ##                                            weightTfIdf(x, normalize =
    ##                                                        FALSE),
    ##                                            stopwords = TRUE))
    ## in example("DocumentTermMatrix") fails [because weightTfIdf() is
    ## a weight function and not a weight function generator ...]
    ## Hence, for now, instead of
    ##   if(inherits(weighting, "WeightFunction"))
    ##      x <- weighting(x)
    ## use
    if(is.function(weighting))
        x <- weighting(x)
    ## and hope for the best ...
    ## </NOTE>
    else if(is.character(weighting) && (length(weighting) == 2L))
        attr(x, "Weighting") <- weighting
    else
        stop("invalid weighting")
    x
}

TermDocumentMatrix <-
function(x, control = list())
    UseMethod("TermDocumentMatrix", x)

TermDocumentMatrix.PCorpus <-
TermDocumentMatrix.VCorpus <-
function(x, control = list())
{
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

    m <- simple_triplet_matrix(i = i, j = j, v = as.numeric(v),
                               nrow = length(allTerms),
                               ncol = length(x),
                               dimnames =
                               list(Terms = allTerms,
                                    Docs = unlist(lapply(x, ID))))

    bg <- control$bounds$global
    if (length(bg) == 2L && is.numeric(bg)) {
        rs <- row_sums(m > 0)
        m <- m[(rs >= bg[1]) & (rs <= bg[2]), ]
    }

    weighting <- control$weighting
    if (is.null(weighting))
        weighting <- weightTf

    .TermDocumentMatrix(m, weighting)
}

DocumentTermMatrix <-
function(x, control = list())
    t(TermDocumentMatrix(x, control))

as.TermDocumentMatrix <-
function(x, ...)
    UseMethod("as.TermDocumentMatrix")
as.TermDocumentMatrix.TermDocumentMatrix <-
function(x, ...)
    x
as.TermDocumentMatrix.DocumentTermMatrix <-
function(x, ...)
    t(x)
as.TermDocumentMatrix.term_frequency <-
function(x, ...) {
    m <- simple_triplet_matrix(i = seq_along(x),
                               j = rep(1, length(x)),
                               v = as.numeric(x),
                               nrow = length(x),
                               ncol = 1,
                               dimnames =
                               list(Terms = names(x),
                                    Docs = NA))

    .TermDocumentMatrix(m, weightTf)
}
as.TermDocumentMatrix.default <-
function(x, weighting, ...)
    .TermDocumentMatrix(x, weighting)

as.DocumentTermMatrix <-
function(x, ...)
    UseMethod("as.DocumentTermMatrix")
as.DocumentTermMatrix.DocumentTermMatrix <-
function(x, ...)
    x
as.DocumentTermMatrix.TermDocumentMatrix <-
function(x, ...)
    t(x)
as.DocumentTermMatrix.default <-
function(x, weighting, ...)
{
    x <- as.simple_triplet_matrix(x)
    t(.TermDocumentMatrix(t(x), weighting))
}

t.TermDocumentMatrix <-
t.DocumentTermMatrix <-
function(x)
{
    m <- slam:::t.simple_triplet_matrix(x)
    attr(m, "Weighting") <- attr(x, "Weighting")
    class(m) <- if(inherits(x, "DocumentTermMatrix"))
        TermDocumentMatrix_classes
    else
        DocumentTermMatrix_classes
    m
}

termFreq <-
function(doc, control = list())
{
    txt <- Content(doc)

    ## Conversion to lower characters
    tolower <- control$tolower
    if (is.null(tolower) || isTRUE(tolower))
        tolower <- base::tolower
    if (is.function(tolower))
        txt <- tolower(txt)

    ## Tokenize the corpus
    tokenize <- control$tokenize
    if (is.null(tokenize) || identical(tokenize, "scan"))
        tokenize <- scan_tokenizer
    else if (identical(tokenize, "MC"))
        tokenize <- MC_tokenizer
    if (is.function(tokenize))
        txt <- tokenize(txt)

    ## Punctuation removal
    removePunctuation <- control$removePunctuation
    if (isTRUE(removePunctuation))
        removePunctuation <- tm::removePunctuation
    else if (is.list(removePunctuation))
        removePunctuation <- function(x) do.call(tm::removePunctuation, c(list(x), control$removePunctuation))

    ## Number removal
    removeNumbers <- control$removeNumbers
    if (isTRUE(removeNumbers))
        removeNumbers <- tm::removeNumbers

    ## Stopword filtering
    stopwords <- control$stopwords
    # Remove stopwords
    rs <- function(x, words) x[is.na(match(x, words))]
    if (isTRUE(stopwords))
        stopwords <- function(x) rs(x, tm::stopwords(Language(doc)))
    else if (is.character(stopwords)) {
        words <- stopwords
        stopwords <- function(x) rs(x, words)
    }

    ## Stemming
    stemming <- control$stemming
    if (isTRUE(stemming))
        stemming <- function(x) stemDocument(x, language = tm:::map_IETF_Snowball(Language(doc)))

    ## Default order for options which support reordering
    or <- c("removePunctuation", "removeNumbers", "stopwords", "stemming")

    ## Process control options in specified order
    nc <- names(control)
    n <- nc[nc %in% or]
    for (name in c(n, setdiff(or, n))) {
        g <- get(name)
        if (is.function(g))
            txt <- g(txt)
    }

    ## Check if the document content is NULL
    if (is.null(txt))
        return(structure(integer(0), names = character(0)))

    ## If dictionary is set tabulate against it
    dictionary <- control$dictionary
    tab <-  if (is.null(dictionary))
        table(txt)
    else
        table(factor(txt, levels = dictionary))

    ## Ensure local bounds
    bl <- control$bounds$local
    if (length(bl) == 2L && is.numeric(bl))
        tab <- tab[(tab >= bl[1]) & (tab <= bl[2])]

    ## Filter out too short or too long terms
    nc <- nchar(names(tab), type = "chars")
    wl <- control$wordLengths
    lb <- if (is.numeric(wl[1])) wl[1] else 3
    ub <- if (is.numeric(wl[2])) wl[2] else Inf
    tab <- tab[(nc >= lb) & (nc <= ub)]

    ## Return named integer
    structure(as.integer(tab), names = names(tab), class = c("term_frequency", "integer"))
}

print.TermDocumentMatrix <-
print.DocumentTermMatrix <-
function(x, ...)
{
    format <- c("term", "document")
    if (inherits(x, "DocumentTermMatrix"))
        format <- rev(format)
    cat(sprintf("A %s-%s matrix (%d %ss, %d %ss)\n",
                format[1L], format[2L], nrow(x),
                format[1L], ncol(x), format[2L]))
    cat(sprintf("\nNon-/sparse entries: %d/%.0f\n",
                length(x$v), prod(dim(x)) - length(x$v)))
    sparsity <- if (identical(prod(dim(x)), 0L)) 100 else round((1 - length(x$v)/prod(dim(x))) * 100)
    cat(sprintf("Sparsity           : %s%%\n", sparsity))
    cat("Maximal term length:", max(nchar(Terms(x), type = "chars"), 0), "\n")
    cat(sprintf("Weighting          : %s (%s)\n",
                attr(x, "Weighting")[1L], attr(x, "Weighting")[2L]))
    invisible(x)
}

inspect.TermDocumentMatrix <-
inspect.DocumentTermMatrix <-
function(x)
{
    print(x)
    cat("\n")
    print(as.matrix(x))
}

`[.TermDocumentMatrix` <-
`[.DocumentTermMatrix` <-
function(x, i, j, ..., drop)
{
    m <- slam:::`[.simple_triplet_matrix`(x, i, j, ...)
    attr(m, "Weighting") <- attr(x, "Weighting")
    class(m) <- if (inherits(x, "DocumentTermMatrix"))
        DocumentTermMatrix_classes
    else
        TermDocumentMatrix_classes
    m
}

`dimnames<-.DocumentTermMatrix` <-
function(x, value)
{
    x <- NextMethod("dimnames<-")
    dnx <- x$dimnames
    if(!is.null(dnx))
        names(dnx) <- c("Docs", "Terms")
    x$dimnames <- dnx
    x
}

`dimnames<-.TermDocumentMatrix` <-
function(x, value)
{
    x <- NextMethod("dimnames<-")
    dnx <- x$dimnames
    if(!is.null(dnx))
        names(dnx) <- c("Terms", "Docs")
    x$dimnames <- dnx
    x
}

nDocs <-
function(x)
    if (inherits(x, "DocumentTermMatrix")) x$nrow else x$ncol
nTerms <-
function(x)
    if (inherits(x, "DocumentTermMatrix")) x$ncol else x$nrow

Docs <-
function(x)
    if (inherits(x, "DocumentTermMatrix")) x$dimnames[[1L]] else x$dimnames[[2L]]
Terms <-
function(x)
    if (inherits(x, "DocumentTermMatrix")) x$dimnames[[2L]] else x$dimnames[[1L]]

c.term_frequency <-
function(x, ..., recursive = FALSE)
{
    args <- list(...)
    x <- as.TermDocumentMatrix(x)

    if(!length(args))
        return(x)

    do.call("c", base::c(list(x), lapply(args, as.TermDocumentMatrix)))
}

c.TermDocumentMatrix <-
function(x, ..., recursive = FALSE)
{
    args <- list(...)

    if(!length(args))
        return(x)

    args <- lapply(args, as.TermDocumentMatrix)

    m <- base::c(list(x), args)
    allTermsNonUnique <- unlist(lapply(m, function(x) Terms(x)[x$i]))
    allTerms <- unique(allTermsNonUnique)
    allDocs <- unlist(lapply(m, Docs))

    cs <- cumsum(lapply(m, nDocs))
    cs <- c(0, cs[-length(cs)])
    j <- lapply(m, "[[", "j")

    m <- simple_triplet_matrix(i = match(allTermsNonUnique, allTerms),
                               j = unlist(j) + rep.int(cs, sapply(j, length)),
                               v = unlist(lapply(m, "[[", "v")),
                               nrow = length(allTerms),
                               ncol = length(allDocs),
                               dimnames =
                               list(Terms = allTerms,
                                    Docs = allDocs))
    ## <NOTE>
    ## - We assume that all arguments have the same weighting
    ## - Even if all matrices have the same input weighting it might be necessary
    ##   to take additional steps (e.g., normalization for tf-idf or check for
    ##   (0,1)-range for binary tf)
    ## </NOTE>
    .TermDocumentMatrix(m, attr(x, "Weighting"))
}

c.DocumentTermMatrix <-
function(x, ..., recursive = FALSE)
{
    args <- list(...)

    if(!length(args))
        return(x)

    t(do.call("c",
              lapply(base::c(list(x), args),
                     as.TermDocumentMatrix)))
}

findFreqTerms <-
function(x, lowfreq = 0, highfreq = Inf)
{
    if (inherits(x, "DocumentTermMatrix")) x <- t(x)
    rs <- slam::row_sums(x)
    names(rs[rs >= lowfreq & rs <= highfreq])
}

findAssocs <-
function(x, term, corlimit)
    UseMethod("findAssocs", x)
findAssocs.TermDocumentMatrix <-
function(x, term, corlimit)
    findAssocs(t(x), term, corlimit)
findAssocs.DocumentTermMatrix <-
function(x, term, corlimit)
{
    ind <- term == Terms(x)
    suppressWarnings(x.cor <- cor(as.matrix(x[, ind]), as.matrix(x[, !ind])))
    findAssocs(x.cor, term, corlimit)
}
findAssocs.matrix <-
function(x, term, corlimit)
    sort(round(x[term, which(x[term,] > corlimit)], 2), decreasing = TRUE)

removeSparseTerms <-
function(x, sparse)
{
    if ((sparse <= 0) || (sparse >= 1))
        stop("invalid sparse factor")
    else {
        m <- if (inherits(x, "DocumentTermMatrix")) t(x) else x
        t <- table(m$i) > m$ncol * (1 - sparse)
        termIndex <- as.numeric(names(t[t]))
        if (inherits(x, "DocumentTermMatrix")) x[, termIndex] else x[termIndex,]
    }
}

CategorizedDocumentTermMatrix <-
function(x, c)
{
    if(inherits(x, "TermDocumentMatrix"))
        x <- t(x)
    else if(!inherits(x, "DocumentTermMatrix"))
        stop("wrong class")

    if(length(c) != nDocs(x))
        stop("invalid category ids")

    attr(x, "Category") <- c

    class(x) <- c("CategorizedDocumentTermMatrix",
                  DocumentTermMatrix_classes)

    x
}
