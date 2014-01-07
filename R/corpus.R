# Author: Ingo Feinerer

.PCorpus <-
function(x, cmeta, dmeta, dbcontrol)
{
    attr(x, "CMetaData") <- cmeta
    attr(x, "DMetaData") <- dmeta
    attr(x, "DBControl") <- dbcontrol
    class(x) <- c("PCorpus", "Corpus", "list")
    x
}

DBControl <-
function(x)
    attr(x, "DBControl")

PCorpus <-
function(x,
         readerControl = list(reader = x$DefaultReader, language = "en"),
         dbControl = list(dbName = "", dbType = "DB1"))
{
    stopifnot(is.Source(x))

    readerControl <- prepareReader(readerControl, x$DefaultReader)

    if (is.function(readerControl$init))
        readerControl$init()

    if (is.function(readerControl$exit))
        on.exit(readerControl$exit())

    if (!filehash::dbCreate(dbControl$dbName, dbControl$dbType))
        stop("error in creating database")
    db <- filehash::dbInit(dbControl$dbName, dbControl$dbType)

    # Allocate memory in advance if length is known
    tdl <- if (x$Length > 0)
        vector("list", as.integer(x$Length))
    else
        list()

    counter <- 1
    while (!eoi(x)) {
        x <- stepNext(x)
        elem <- getElem(x)
        id <- if (is.null(x$Names) || is.na(x$Names))
                as.character(counter)
            else
                x$Names[counter]
        doc <- readerControl$reader(elem, readerControl$language, id)
        filehash::dbInsert(db, ID(doc), doc)
        if (x$Length > 0) tdl[[counter]] <- ID(doc)
        else tdl <- c(tdl, ID(doc))
        counter <- counter + 1
    }
    if (!is.null(x$Names) && !is.na(x$Names))
        names(tdl) <- x$Names

    df <- data.frame(MetaID = rep(0, length(tdl)), stringsAsFactors = FALSE)
    filehash::dbInsert(db, "DMetaData", df)
    dmeta.df <- data.frame(key = "DMetaData", subset = I(list(NA)))

    .PCorpus(tdl, .MetaDataNode(), dmeta.df, dbControl)
}

.VCorpus <-
function(x, cmeta, dmeta)
{
    attr(x, "CMetaData") <- cmeta
    attr(x, "DMetaData") <- dmeta
    class(x) <- c("VCorpus", "Corpus", "list")
    x
}

VCorpus <-
Corpus <-
function(x, readerControl = list(reader = x$DefaultReader, language = "en"))
{
    stopifnot(is.Source(x))

    readerControl <- prepareReader(readerControl, x$DefaultReader)

    if (is.function(readerControl$init))
        readerControl$init()

    if (is.function(readerControl$exit))
        on.exit(readerControl$exit())

    # Allocate memory in advance if length is known
    tdl <- if (x$Length > 0)
        vector("list", as.integer(x$Length))
    else
        list()

    if (x$Vectorized)
        tdl <- mapply(function(x, id) readerControl$reader(x, readerControl$language, id),
                      pGetElem(x),
                      id = if (is.null(x$Names) || is.na(x$Names)) as.character(seq_len(x$Length)) else x$Names,
                      SIMPLIFY = FALSE)
    else {
        counter <- 1
        while (!eoi(x)) {
            x <- stepNext(x)
            elem <- getElem(x)
            id <- if (is.null(x$Names) || is.na(x$Names))
                as.character(counter)
            else
                x$Names[counter]
            doc <- readerControl$reader(elem, readerControl$language, id)
            if (x$Length > 0)
                tdl[[counter]] <- doc
            else
                tdl <- c(tdl, list(doc))
            counter <- counter + 1
        }
    }
    if (!is.null(x$Names) && !is.na(x$Names))
        names(tdl) <- x$Names
    df <- data.frame(MetaID = rep(0, length(tdl)), stringsAsFactors = FALSE)
    .VCorpus(tdl, .MetaDataNode(), df)
}

`[.PCorpus` <-
function(x, i)
{
    if (missing(i)) return(x)
    index <- attr(x, "DMetaData")[[1 , "subset"]]
    attr(x, "DMetaData")[[1 , "subset"]] <- if (is.numeric(index)) index[i] else i
    dmeta <- attr(x, "DMetaData")
    .PCorpus(NextMethod("["), CMetaData(x), dmeta, DBControl(x))
}

`[.VCorpus` <-
function(x, i)
{
    if (missing(i)) return(x)
    .VCorpus(NextMethod("["), CMetaData(x), DMetaData(x)[i, , drop = FALSE])
}

`[<-.PCorpus` <-
function(x, i, value)
{
    db <- filehash::dbInit(DBControl(x)[["dbName"]], DBControl(x)[["dbType"]])
    counter <- 1
    for (id in unclass(x)[i]) {
        if (identical(length(value), 1L)) db[[id]] <- value
        else db[[id]] <- value[[counter]]
        counter <- counter + 1
    }
    x
}

.map_name_index <-
function(x, i)
{
    if (is.character(i)) {
        if (is.null(names(x)))
            match(i, meta(x, "ID", type = "local"))
        else
            match(i, names(x))
    }
    i
}

`[[.PCorpus` <-
function(x, i)
{
    i <- .map_name_index(x, i)
    db <- filehash::dbInit(DBControl(x)[["dbName"]], DBControl(x)[["dbType"]])
    filehash::dbFetch(db, NextMethod("[["))
}
`[[.VCorpus` <-
function(x, i)
{
    i <- .map_name_index(x, i)
    lazyTmMap <- meta(x, tag = "lazyTmMap", type = "corpus")
    if (!is.null(lazyTmMap))
        .Call("copyCorpus", x, materialize(x, i))
    NextMethod("[[")
}

`[[<-.PCorpus` <-
function(x, i, value)
{
    i <- .map_name_index(x, i)
    db <- filehash::dbInit(DBControl(x)[["dbName"]], DBControl(x)[["dbType"]])
    index <- unclass(x)[[i]]
    db[[index]] <- value
    x
}
`[[<-.VCorpus` <-
function(x, i, value)
{
    i <- .map_name_index(x, i)
    # Mark new objects as not active for lazy mapping
    lazyTmMap <- meta(x, tag = "lazyTmMap", type = "corpus")
    if (!is.null(lazyTmMap)) {
        lazyTmMap$index[i] <- FALSE
        meta(x, tag = "lazyTmMap", type = "corpus") <- lazyTmMap
    }
    # Set the value
    cl <- class(x)
    y <- NextMethod("[[<-")
    class(y) <- cl
    y
}

# Update NodeIDs of a CMetaData tree
.update_id <-
function(x, id = 0, mapping = NULL, left.mapping = NULL, level = 0)
{
    # Traversal of (binary) CMetaData tree with setup of NodeIDs
    set_id <- function(x) {
        x$NodeID <- id
        id <<- id + 1
        level <<- level + 1
        if (length(x$Children)) {
            mapping <<- cbind(mapping, c(x$Children[[1]]$NodeID, id))
            left <- set_id(x$Children[[1]])
            if (level == 1) {
                left.mapping <<- mapping
                mapping <<- NULL
            }
            mapping <<- cbind(mapping, c(x$Children[[2]]$NodeID, id))
            right <- set_id(x$Children[[2]])

            x$Children <- list(left, right)
        }
        level <<- level - 1
        x
    }
    list(root = set_id(x), left.mapping = left.mapping, right.mapping = mapping)
}

# Find indices to be updated for a CMetaData tree
.find_indices <-
function(x)
{
    indices.mapping <- NULL
    for (m in levels(as.factor(DMetaData(x)$MetaID))) {
        indices <- (DMetaData(x)$MetaID == m)
        indices.mapping <- c(indices.mapping, list(m = indices))
        names(indices.mapping)[length(indices.mapping)] <- m
    }
    indices.mapping
}

c2 <-
function(x, y, ...)
{
    # Update the CMetaData tree
    cmeta <- .MetaDataNode(0, list(merge_date = as.POSIXlt(Sys.time(), tz = "GMT"), merger = Sys.getenv("LOGNAME")), list(CMetaData(x), CMetaData(y)))
    update.struct <- .update_id(cmeta)

    new <- .VCorpus(c(unclass(x), unclass(y)), update.struct$root, NULL)

    # Find indices to be updated for the left tree
    indices.mapping <- .find_indices(x)

    # Update the DMetaData data frames for the left tree
    for (i in 1:ncol(update.struct$left.mapping)) {
        map <- update.struct$left.mapping[,i]
        DMetaData(x)$MetaID <- replace(DMetaData(x)$MetaID, indices.mapping[[as.character(map[1])]], map[2])
    }

    # Find indices to be updated for the right tree
    indices.mapping <- .find_indices(y)

    # Update the DMetaData data frames for the right tree
    for (i in 1:ncol(update.struct$right.mapping)) {
        map <- update.struct$right.mapping[,i]
        DMetaData(y)$MetaID <- replace(DMetaData(y)$MetaID, indices.mapping[[as.character(map[1])]], map[2])
    }

    # Merge the DMetaData data frames
    labels <- setdiff(names(DMetaData(y)), names(DMetaData(x)))
    na.matrix <- matrix(NA,
                        nrow = nrow(DMetaData(x)),
                        ncol = length(labels),
                        dimnames = list(row.names(DMetaData(x)), labels))
    x.dmeta.aug <- cbind(DMetaData(x), na.matrix)
    labels <- setdiff(names(DMetaData(x)), names(DMetaData(y)))
    na.matrix <- matrix(NA,
                        nrow = nrow(DMetaData(y)),
                        ncol = length(labels),
                        dimnames = list(row.names(DMetaData(y)), labels))
    y.dmeta.aug <- cbind(DMetaData(y), na.matrix)
    DMetaData(new) <- rbind(x.dmeta.aug, y.dmeta.aug)

    new
}

c.Corpus <-
function(..., recursive = FALSE)
{
    args <- list(...)
    x <- args[[1L]]

    if(length(args) == 1L)
        return(x)

    if (!all(unlist(lapply(args, inherits, class(x)))))
        stop("not all arguments are of the same corpus type")

    if (inherits(x, "PCorpus"))
        stop("concatenation of corpora with underlying databases is not supported")

    if (recursive)
        Reduce(c2, args)
    else {
        args <- do.call("c", lapply(args, unclass))
        .VCorpus(args,
                 cmeta = .MetaDataNode(),
                 dmeta = data.frame(MetaID = rep(0, length(args)),
                                    stringsAsFactors = FALSE))
    }
}

c.TextDocument <-
function(..., recursive = FALSE)
{
    args <- list(...)
    x <- args[[1L]]

    if(length(args) == 1L)
        return(x)

    if (!all(unlist(lapply(args, inherits, class(x)))))
        stop("not all arguments are text documents")

    dmeta <- data.frame(MetaID = rep(0, length(args)),
                        stringsAsFactors = FALSE)
    .VCorpus(args, .MetaDataNode(), dmeta)
}

print.Corpus <-
function(x, ...)
{
    cat(sprintf(ngettext(length(x),
                         "A corpus with %d text document\n",
                         "A corpus with %d text documents\n"),
                length(x)))
    invisible(x)
}

summary.Corpus <-
function(object, ...)
{
    print(object)
    if (length(DMetaData(object))) {
        cat(sprintf(ngettext(length(attr(CMetaData(object), "MetaData")),
                             "\nThe metadata consists of %d tag-value pair and a data frame\n",
                             "\nThe metadata consists of %d tag-value pairs and a data frame\n"),
                    length(CMetaData(object)$MetaData)))
        cat("Available tags are:\n")
        cat(strwrap(paste(names(CMetaData(object)$MetaData), collapse = " "), indent = 2, exdent = 2), "\n")
        cat("Available variables in the data frame are:\n")
        cat(strwrap(paste(names(DMetaData(object)), collapse = " "), indent = 2, exdent = 2), "\n")
    }
}

inspect <-
function(x)
    UseMethod("inspect", x)
inspect.PCorpus <-
function(x)
{
    summary(x)
    cat("\n")
    db <- filehash::dbInit(DBControl(x)[["dbName"]], DBControl(x)[["dbType"]])
    show(filehash::dbMultiFetch(db, unlist(x)))
}
inspect.VCorpus <-
function(x)
{
    summary(x)
    cat("\n")
    print(noquote(lapply(x, identity)))
}

lapply.PCorpus <-
function(X, FUN, ...)
{
    db <- filehash::dbInit(DBControl(X)[["dbName"]], DBControl(X)[["dbType"]])
    lapply(filehash::dbMultiFetch(db, unlist(X)), FUN, ...)
}
lapply.VCorpus <-
function(X, FUN, ...)
{
    lazyTmMap <- meta(X, tag = "lazyTmMap", type = "corpus")
    if (!is.null(lazyTmMap))
        .Call("copyCorpus", X, materialize(X))
    base::lapply(X, FUN, ...)
}

writeCorpus <-
function(x, path = ".", filenames = NULL)
{
    filenames <- file.path(path,
                           if (is.null(filenames)) unlist(lapply(x, function(x) sprintf("%s.txt", ID(x))))
                           else filenames)
    i <- 1
    for (o in x) {
        writeLines(as.PlainTextDocument(o), filenames[i])
        i <- i + 1
    }
}
