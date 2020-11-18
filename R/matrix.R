## Authors: Ingo Feinerer, Kurt Hornik

TermDocumentMatrix_classes <-
    c("TermDocumentMatrix", "simple_triplet_matrix")
DocumentTermMatrix_classes <-
    c("DocumentTermMatrix", "simple_triplet_matrix")

.TermDocumentMatrix <-
function(x, weighting)
{
    x <- as.simple_triplet_matrix(x)
    if (!is.null(dimnames(x)))
        names(dimnames(x)) <- c("Terms", "Docs")
    class(x) <- TermDocumentMatrix_classes

    if (is.null(weighting))
        weighting <- weightTf
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
    ##   if (inherits(weighting, "WeightFunction"))
    ##      x <- weighting(x)
    ## use
    if (is.function(weighting))
        x <- weighting(x)
    ## and hope for the best ...
    ## </NOTE>
    else if (is.character(weighting) && (length(weighting) == 2L))
        attr(x, "weighting") <- weighting
    x
}

.SimpleTripletMatrix <-
function(i, j, v, terms, corpus)
{
    docs <- as.character(meta(corpus, "id", "local"))
    if (length(docs) != length(corpus)) {
        warning("invalid document identifiers")
        docs <- NULL
    }

    simple_triplet_matrix(i, j, v,
                          nrow = length(terms),
                          ncol = length(corpus),
                          dimnames = list(Terms = terms, Docs = docs))
}

filter_global_bounds <-
function(m, bounds)
{
    m <- as.simple_triplet_matrix(m)

    if (length(bounds) == 2L && is.numeric(bounds)) {
        rs <- row_sums(m > 0)
        m <- m[(rs >= bounds[1]) & (rs <= bounds[2]), ]
    }
    m
}

TermDocumentMatrix <-
function(x, control = list())
    UseMethod("TermDocumentMatrix", x)

TermDocumentMatrix.SimpleCorpus <-
function(x, control = list())
{
    stopifnot(is.list(control))

    if (any(unlist(lapply(control, is.function))))
        warning("custom functions are ignored")

    if (!is.null(control$tokenize) && !identical(control$tokenize, "Boost"))
        warning("custom tokenizer is ignored")

    txt <- content(x)

    ## Conversion to lower case
    if (is.null(control$tolower) || isTRUE(control$tolower))
        txt <- tolower(txt)

    ## Stopword filtering
    .stopwords <- if (isTRUE(control$stopwords)) stopwords(meta(x, "language"))
        else if (is.character(control$stopwords)) control$stopwords
        else character(0)

    .dictionary <- if (is.null(control$dictionary)) character(0)
        else control$dictionary

    ## Ensure local bounds
    bl <- control$bounds$local
    min_term_freq <-
        if (length(bl) == 2L && is.numeric(bl) && bl[1] >= 0) bl[1] else 0L
    max_term_freq <-
        if (length(bl) == 2L && is.numeric(bl) && bl[2] >= 0)
            min(bl[2], .Machine$integer.max) else .Machine$integer.max

    ## Filter out too short or too long terms
    wl <- control$wordLengths
    min_word_length <- if (is.numeric(wl[1]) && wl[1] >= 0) wl[1] else 3L
    max_word_length <- if (is.numeric(wl[2]) && wl[2] >= 0)
        min(wl[2], .Machine$integer.max) else .Machine$integer.max

    m <- tdm(txt,
             isTRUE(control$removePunctuation),
             isTRUE(control$removeNumbers),
             .stopwords, .dictionary,
             as.integer(min_term_freq), as.integer(max_term_freq),
             as.integer(min_word_length), as.integer(max_word_length))

    Encoding(m$terms) <- "UTF-8"
    m <- .SimpleTripletMatrix(m$i, m$j, m$v, m$terms, x)

    ## Stemming
    ## <NOTE>
    ## Ideally tdm() could perform stemming as well but there is no easy way to
    ## access the SnowballC::wordStem() function from C++ (via Rcpp) without
    ## significant overhead (as SnowballC does not export its internal C
    ## functions).
    ##
    ## Stemming afterwards is still quite performant as we already have
    ## all terms.  However, there is some overhead involved as we need
    ## to recheck local bounds and word lengths.
    ## </NOTE>
    if (isTRUE(control$stemming)) {
        stems <- SnowballC::wordStem(m$dimnames$Terms,
                                     meta(x, "language"))
        ## Do as.factor(stems) "by hand" for performance reasons.
        uniqs <- sort(unique(stems))
        stems <- match(stems, uniqs)
        attributes(stems) <- list(levels = uniqs, class = "factor")
        m <- rollup(m, "Terms", stems)

        ## Recheck local bounds
        ## No need to check lower local bound as rollup aggregates frequencies
        m[m > max_term_freq] <- 0

        ## Recheck word lengths
        terms_length <- nchar(rownames(m))
        m <- m[min_word_length <= terms_length &
               terms_length <= max_word_length, ]
    }

    m <- filter_global_bounds(m, control$bounds$global)

    .TermDocumentMatrix(m, control$weighting)
}

TermDocumentMatrix.PCorpus <-
TermDocumentMatrix.VCorpus <-
function(x, control = list())
{
    stopifnot(is.list(control))

    tflist <- tm_parLapply(unname(content(x)), termFreq, control)

    v <- unlist(tflist)
    i <- names(v)
    terms <- sort(unique(as.character(if (is.null(control$dictionary)) i
                                      else control$dictionary)))
    i <- match(i, terms)
    j <- rep.int(seq_along(x), lengths(tflist))

    m <- .SimpleTripletMatrix(i, j, as.numeric(v), terms, x)
    m <- filter_global_bounds(m, control$bounds$global)
    .TermDocumentMatrix(m, control$weighting)
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
as.TermDocumentMatrix.textcnt <-
function(x, ...)
{
    m <- simple_triplet_matrix(i = seq_along(x),
                               j = rep_len(1L, length(x)),
                               v = as.numeric(x),
                               nrow = length(x),
                               ncol = 1,
                               dimnames =
                               list(Terms = names(x),
                                    Docs = NA_character_))

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
as.DocumentTermMatrix.term_frequency <-
as.DocumentTermMatrix.textcnt <-
function(x, ...)
    t(as.TermDocumentMatrix(x))
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
    m <- NextMethod("t")
    attr(m, "weighting") <- attr(x, "weighting")
    class(m) <- if (inherits(x, "DocumentTermMatrix"))
        TermDocumentMatrix_classes
    else
        DocumentTermMatrix_classes
    m
}

termFreq <-
function(doc, control = list())
{
    stopifnot(inherits(doc, "TextDocument") || is.character(doc),
              is.list(control))

    ## Tokenize the corpus
    .tokenize <- control$tokenize
    if (is.null(.tokenize) || identical(.tokenize, "words"))
        .tokenize <- words
    else if (identical(.tokenize, "Boost"))
        .tokenize <- Boost_tokenizer
    else if (identical(.tokenize, "MC"))
        .tokenize <- MC_tokenizer
    else if (identical(.tokenize, "scan"))
        .tokenize <- scan_tokenizer
    else if (is.Span_Tokenizer(.tokenize))
        .tokenize <- as.Token_Tokenizer(.tokenize)
    if (is.function(.tokenize))
        txt <- .tokenize(doc)
    else
        stop("invalid tokenizer")

    ## Conversion to lower case
    .tolower <- control$tolower
    if (is.null(.tolower) || isTRUE(.tolower))
        .tolower <- tolower
    if (is.function(.tolower))
        txt <- .tolower(txt)

    ## Punctuation removal
    .removePunctuation <- control$removePunctuation
    if (isTRUE(.removePunctuation))
        .removePunctuation <- removePunctuation
    else if (is.list(.removePunctuation))
        .removePunctuation <-
            function(x) do.call(removePunctuation,
                                c(list(x), control$removePunctuation))

    ## Number removal
    .removeNumbers <- control$removeNumbers
    if (isTRUE(.removeNumbers))
        .removeNumbers <- removeNumbers

    .language <- control$language
    if (inherits(doc, "TextDocument"))
        .language <- meta(doc, "language")
    if (is.null(.language))
        .language <- "en"

    ## Stopword filtering
    .stopwords <- control$stopwords
    if (isTRUE(.stopwords))
        .stopwords <- function(x) x[is.na(match(x, stopwords(.language)))]
    else if (is.character(.stopwords))
        .stopwords <- function(x) x[is.na(match(x, control$stopwords))]

    ## Stemming
    .stemming <- control$stemming
    if (isTRUE(.stemming))
        .stemming <- function(x) SnowballC::wordStem(x, .language)

    ## Default order for options which support reordering
    or <- c("removePunctuation", "removeNumbers", "stopwords", "stemming")

    ## Process control options in specified order
    nc <- names(control)
    n <- nc[!is.na(match(nc, or))]
    for (name in sprintf(".%s", c(n, setdiff(or, n)))) {
        g <- get(name)
        if (is.function(g))
            txt <- g(txt)
    }

    ## If dictionary is set tabulate against it
    dictionary <- control$dictionary
    tab <- .table(if (is.null(dictionary))
                      txt
                  else
                      txt[!is.na(match(txt, dictionary))])

    ## Ensure local bounds
    bl <- control$bounds$local
    if (length(bl) == 2L && is.numeric(bl))
        tab <- tab[(tab >= bl[1]) & (tab <= bl[2]), drop = FALSE]

    ## Filter out too short or too long terms
    nc <- nchar(names(tab), type = "chars")
    wl <- control$wordLengths
    lb <- if (is.numeric(wl[1])) wl[1] else 3
    ub <- if (is.numeric(wl[2])) wl[2] else Inf
    tab <- tab[(nc >= lb) & (nc <= ub), drop = FALSE]

    class(tab) <- c("term_frequency", class(tab))
    tab
}

print.TermDocumentMatrix <-
print.DocumentTermMatrix <-
function(x, ...)
{
    format <- c("term", "document")
    if (inherits(x, "DocumentTermMatrix"))
        format <- rev(format)
    writeLines(sprintf("<<%s (%ss: %d, %ss: %d)>>",
                       class(x)[1], format[1L], nrow(x), format[2L], ncol(x)))
    writeLines(sprintf("Non-/sparse entries: %d/%.0f",
                       length(x$v), prod(dim(x)) - length(x$v)))
    sparsity <- if (!prod(dim(x))) 100
        else round( (1 - length(x$v) / prod(dim(x))) * 100)
    writeLines(sprintf("Sparsity           : %s%%", sparsity))
    writeLines(sprintf("Maximal term length: %s",
                       max(nchar(Terms(x), type = "chars"), 0)))
    writeLines(sprintf("Weighting          : %s (%s)",
                       attr(x, "weighting")[1L], attr(x, "weighting")[2L]))
    invisible(x)
}

inspect.TermDocumentMatrix <-
inspect.DocumentTermMatrix <-
function(x)
{
    print(x)
    cat("Sample             :\n")
    print(as.matrix(sample.TermDocumentMatrix(x)))
}

`[.TermDocumentMatrix` <-
`[.DocumentTermMatrix` <-
function(x, i, j, ..., drop)
{
    m <- NextMethod("[")
    attr(m, "weighting") <- attr(x, "weighting")
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
    if (!is.null(dnx))
        names(dnx) <- c("Docs", "Terms")
    x$dimnames <- dnx
    x
}

`dimnames<-.TermDocumentMatrix` <-
function(x, value)
{
    x <- NextMethod("dimnames<-")
    dnx <- x$dimnames
    if (!is.null(dnx))
        names(dnx) <- c("Terms", "Docs")
    x$dimnames <- dnx
    x
}

nDocs <-
function(x)
    UseMethod("nDocs")

nTerms <-
function(x)
    UseMethod("nTerms")

nDocs.DocumentTermMatrix <-
nTerms.TermDocumentMatrix <-
function(x)
    x$nrow

nDocs.TermDocumentMatrix <-
nTerms.DocumentTermMatrix <-
function(x)
    x$ncol

Docs <-
function(x)
    UseMethod("Docs")

Terms <-
function(x)
    UseMethod("Terms")

Docs.DocumentTermMatrix <-
Terms.TermDocumentMatrix <-
function(x)
{
    s <- x$dimnames[[1L]]
    if (is.null(s))
        s <- rep.int(NA_character_, x$nrow)
    s
}

Docs.TermDocumentMatrix <-
Terms.DocumentTermMatrix <-
function(x)
{
    s <- x$dimnames[[2L]]
    if (is.null(s))
        s <- rep.int(NA_character_, x$ncol)
    s
}

c.term_frequency <-
function(..., recursive = FALSE)
{
    do.call("c", lapply(list(...), as.TermDocumentMatrix))
}

c.TermDocumentMatrix <-
function(..., recursive = FALSE)
{
    m <- lapply(list(...), as.TermDocumentMatrix)

    if (length(m) == 1L)
        return(m[[1L]])

    weighting <- attr(m[[1L]], "weighting")

    allTermsNonUnique <- unlist(lapply(m, function(x) Terms(x)[x$i]))
    allTerms <- unique(allTermsNonUnique)
    allDocs <- unlist(lapply(m, Docs))

    cs <- cumsum(lapply(m, nDocs))
    cs <- c(0, cs[-length(cs)])
    j <- lapply(m, "[[", "j")

    m <- simple_triplet_matrix(i = match(allTermsNonUnique, allTerms),
                               j = unlist(j) + rep.int(cs, lengths(j)),
                               v = unlist(lapply(m, "[[", "v")),
                               nrow = length(allTerms),
                               ncol = length(allDocs),
                               dimnames =
                               list(Terms = allTerms,
                                    Docs = allDocs))
    ## <NOTE>
    ## - We assume that all arguments have the same weighting
    ## - Even if all matrices have the same input weighting it might be
    ##   necessary to take additional steps (e.g., normalization for tf-idf or
    ##   check for (0,1)-range for binary tf)
    ## </NOTE>
    .TermDocumentMatrix(m, weighting)
}

c.DocumentTermMatrix <-
function(..., recursive = FALSE)
{
    t(do.call("c", lapply(list(...), as.TermDocumentMatrix)))
}

findFreqTerms <-
function(x, lowfreq = 0, highfreq = Inf)
{
    stopifnot(inherits(x, c("DocumentTermMatrix", "TermDocumentMatrix")),
              is.numeric(lowfreq), is.numeric(highfreq))

    if (inherits(x, "DocumentTermMatrix")) x <- t(x)
    rs <- row_sums(x)
    names(rs[rs >= lowfreq & rs <= highfreq])
}

findAssocs <-
function(x, terms, corlimit)
    UseMethod("findAssocs", x)
findAssocs.TermDocumentMatrix <-
function(x, terms, corlimit)
    findAssocs(t(x), terms, corlimit)
findAssocs.DocumentTermMatrix <-
function(x, terms, corlimit)
{
    stopifnot(is.character(terms), is.numeric(corlimit),
              corlimit >= 0, corlimit <= 1)

    j <- match(unique(terms), Terms(x), nomatch = 0L)
    suppressWarnings(
        findAssocs(crossapply_simple_triplet_matrix(x[, j], x[, -j], cor),
                   terms, rep_len(corlimit, length(terms))))
}
findAssocs.matrix <-
function(x, terms, corlimit)
{
    stopifnot(is.numeric(x))

    i <- match(terms, rownames(x), nomatch = 0L)
    names(i) <- terms
    Map(function(i, cl) {
        xi <- x[i, ]
        t <- sort(round(xi[which(xi >= cl)], 2), TRUE)
        if (!length(t))
            names(t) <- NULL
        t
        },
        i, corlimit)
}

removeSparseTerms <-
function(x, sparse)
{
    stopifnot(inherits(x, c("DocumentTermMatrix", "TermDocumentMatrix")),
              is.numeric(sparse), sparse > 0, sparse < 1)

    m <- if (inherits(x, "DocumentTermMatrix")) t(x) else x
    t <- table(m$i) > m$ncol * (1 - sparse)
    termIndex <- as.numeric(names(t[t]))
    if (inherits(x, "DocumentTermMatrix")) x[, termIndex] else x[termIndex, ]
}

sample.TermDocumentMatrix <-
function(x, size = 10)
{
    stopifnot(inherits(x, c("DocumentTermMatrix", "TermDocumentMatrix")),
              is.numeric(size), size >= 0)

    if (length(x$v) == 0L)
        return(x)

    m <- if (inherits(x, "DocumentTermMatrix")) t(x) else x
    terms <- sort(names(sort(row_sums(m), decreasing = TRUE)
                        [0:min(size, nTerms(m))]))
    docs <- sort(names(sort(col_sums(m), decreasing = TRUE)
                       [0:min(size, nDocs(m))]))
    if (inherits(x, "DocumentTermMatrix")) x[docs, terms] else x[terms, docs]
}

CategorizedDocumentTermMatrix <-
function(x, c)
{
    if (inherits(x, "TermDocumentMatrix"))
        x <- t(x)
    else if (!inherits(x, "DocumentTermMatrix"))
        stop("wrong class")

    if (length(c) != nDocs(x))
        stop("invalid category ids")

    attr(x, "Category") <- c

    class(x) <- c("CategorizedDocumentTermMatrix",
                  DocumentTermMatrix_classes)

    x
}

findMostFreqTerms <-
function(x, n = 6L, ...)
    UseMethod("findMostFreqTerms")

findMostFreqTerms.term_frequency <-
function(x, n = 6L, ...)
{
    y <- x[order(x, decreasing = TRUE)[seq_len(n)]]
    y[y > 0]
}

findMostFreqTerms.DocumentTermMatrix <-
function(x, n = 6L, INDEX = NULL, ...)
{
    terms <- Terms(x)
    if (!is.null(INDEX))
        x <- rollup(x, 1L, INDEX)
    f <- factor(x$i, seq_len(x$nrow))
    js <- split(x$j, f)
    vs <- split(x$v, f)
    y <- Map(function(j, v, n) {
                 p <- order(v, decreasing = TRUE)[seq_len(n)]
                 v <- v[p]
                 names(v) <- terms[j[p]]
                 v
             },
             js, vs, pmin(lengths(vs), n))
    names(y) <- x$dimnames[[1L]]
    y
}

findMostFreqTerms.TermDocumentMatrix <-
function(x, n = 6L, INDEX = NULL, ...)
{
    terms <- Terms(x)
    if (!is.null(INDEX))
        x <- rollup(x, 2L, INDEX)
    f <- factor(x$j, seq_len(x$ncol))
    is <- split(x$i, f)
    vs <- split(x$v, f)
    y <- Map(function(i, v, n) {
                 p <- order(v, decreasing = TRUE)[seq_len(n)]
                 v <- v[p]
                 names(v) <- terms[i[p]]
                 v
             },
             is, vs, pmin(lengths(vs), n))
    names(y) <- x$dimnames[[2L]]
    y
}
