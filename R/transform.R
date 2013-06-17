# Author: Ingo Feinerer
# Transformations

tm_map <- function(x, FUN, ..., useMeta = FALSE, lazy = FALSE) UseMethod("tm_map", x)
tm_map.VCorpus <- function(x, FUN, ..., useMeta = FALSE, lazy = FALSE) {
    result <- x
    # Lazy mapping
    if (lazy) {
        lazyTmMap <- meta(x, tag = "lazyTmMap", type = "corpus")
        local_fun <- local({
            useMeta <- useMeta
            function(x, ..., DMetaData) {
                if (useMeta)
                    FUN(x, ..., DMetaData = DMetaData)
                else
                    FUN(x, ...)
            }
        })
        if (is.null(lazyTmMap)) {
            meta(result, tag = "lazyTmMap", type = "corpus") <-
                list(index = rep(TRUE, length(result)), maps = list(local_fun))
        }
        else {
            lazyTmMap$maps <- c(lazyTmMap$maps, list(local_fun))
            meta(result, tag = "lazyTmMap", type = "corpus") <- lazyTmMap
        }
    }
    else {
        Content(result) <- if (useMeta)
                parallel::mclapply(x, FUN, ..., DMetaData = DMetaData(x))
            else
                parallel::mclapply(x, FUN, ...)
    }
    result
}
tm_map.PCorpus <- function(x, FUN, ..., useMeta = FALSE, lazy = FALSE) {
    if (lazy)
        warning("lazy mapping is deactived when using database backend")
    db <- filehash::dbInit(DBControl(x)[["dbName"]], DBControl(x)[["dbType"]])
    i <- 1
    for (id in unlist(x)) {
        db[[id]] <- if (useMeta)
            FUN(x[[i]], ..., DMetaData = DMetaData(x))
        else
            FUN(x[[i]], ...)
        i <- i + 1
    }
    # Suggested by Christian Buchta
    filehash::dbReorganize(db)

    x
}

# Materialize lazy mappings
# Improvements by Christian Buchta
materialize <- function(corpus, range = seq_along(corpus)) {
    lazyTmMap <- meta(corpus, tag = "lazyTmMap", type = "corpus")
    if (!is.null(lazyTmMap)) {
       # Make valid and lazy index
       idx <- (seq_along(corpus) %in% range) & lazyTmMap$index
       if (any(idx)) {
           res <- unclass(corpus)[idx]
           for (m in lazyTmMap$maps)
               res <- lapply(res, m, DMetaData = DMetaData(corpus))
           corpus[idx] <- res
           lazyTmMap$index[idx] <- FALSE
       }
    }
    # Clean up if everything is materialized
    if (!any(lazyTmMap$index))
        lazyTmMap <- NULL
    meta(corpus, tag = "lazyTmMap", type = "corpus") <- lazyTmMap
    corpus
}

tm_reduce <- function(x, tmFuns, ...)
    Reduce(function(f, ...) f(...), tmFuns, x, right = TRUE)

getTransformations <- function()
    c("as.PlainTextDocument", "removeNumbers", "removePunctuation",
      "removeWords", "stemDocument", "stripWhitespace")

as.PlainTextDocument <- function(x) UseMethod("as.PlainTextDocument", x)
as.PlainTextDocument.PlainTextDocument <- identity
as.PlainTextDocument.RCV1Document <- function(x) {
    Content(x) <- unlist(XML::xmlApply(XML::xmlRoot(x)[["text"]], XML::xmlValue), use.names = FALSE)
    class(x) <- c("PlainTextDocument", "TextDocument", "character")
    x
}
as.PlainTextDocument.Reuters21578Document <- function(x) {
    Content(x) <- unlist(XML::xmlApply(XML::xmlRoot(x)[["TEXT"]], XML::xmlValue), use.names = FALSE)
    class(x) <- c("PlainTextDocument", "TextDocument", "character")
    x
}

removeNumbers <- function(x) UseMethod("removeNumbers", x)
removeNumbers.PlainTextDocument <- removeNumbers.character <- function(x) gsub("[[:digit:]]+", "", x)

removePunctuation <- function(x, preserve_intra_word_dashes = FALSE) UseMethod("removePunctuation", x)
removePunctuation.PlainTextDocument <- removePunctuation.character <-
function(x, preserve_intra_word_dashes = FALSE)
{
    if (!preserve_intra_word_dashes)
        gsub("[[:punct:]]+", "", x)
    else {
        # Assume there are no ASCII 1 characters.
        x <- gsub("(\\w)-(\\w)", "\\1\1\\2", x)
        x <- gsub("[[:punct:]]+", "", x)
        gsub("\1", "-", x, fixed = TRUE)
    }
}

removeWords <- function(x, words) UseMethod("removeWords", x)
# Improvements by Kurt Hornik
removeWords.PlainTextDocument <- removeWords.character <- function(x, words)
    gsub(sprintf("(*UCP)\\b(%s)\\b", paste(words, collapse = "|")), "", x, perl = TRUE)

stemDocument <- function(x, language = "english") UseMethod("stemDocument", x)
stemDocument.character <- function(x, language = "english")
    SnowballC::wordStem(x, language)
stemDocument.PlainTextDocument <- function(x, language = Language(x)) {
    s <- unlist(lapply(x, function(x) paste(stemDocument.character(unlist(strsplit(x, "[[:blank:]]")), language), collapse = " ")))
    Content(x) <- if (is.character(s)) s else ""
    x
}

stripWhitespace <- function(x) UseMethod("stripWhitespace", x)
stripWhitespace.PlainTextDocument <- stripWhitespace.character <- function(x) gsub("[[:space:]]+", " ", x)
