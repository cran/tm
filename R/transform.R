# Author: Ingo Feinerer
# Transformations

tm_map <-
function(x, FUN, ...)
    UseMethod("tm_map", x)
tm_map.VCorpus <-
function(x, FUN, ..., lazy = FALSE)
{
    # Lazy mapping
    if (lazy) {
        fun <- function(x) FUN(x, ...)
        if (is.null(x$lazy))
            x$lazy <- list(index = rep_len(TRUE, length(x)), maps = list(fun))
        else
            x$lazy$maps <- c(x$lazy$maps, list(fun))
    } else
        x$content <- tm_parLapply(content(x), FUN, ...)
    x
}
tm_map.SimpleCorpus <-
function(x, FUN, ...)
{
    if (inherits(FUN, "content_transformer"))
        FUN <- get("FUN", envir = environment(FUN))

    n <- names(content(x))
    x$content <- FUN(content(x), ...)
    if (length(content(x)) != length(n))
        warning("transformation drops documents")
    else
        names(x$content) <- n
    x
}
tm_map.PCorpus <-
function(x, FUN, ...)
{
    db <- filehash::dbInit(x$dbcontrol[["dbName"]], x$dbcontrol[["dbType"]])
    for (i in seq_along(x))
        db[[x$content[[i]]]] <- FUN(x[[i]], ...)
    filehash::dbReorganize(db)
    x
}

# Materialize lazy mappings
materialize <-
function(x, range = seq_along(x))
{
    if (!is.null(x$lazy)) {
       i <- (seq_along(x) %in% range) & x$lazy$index
       if (any(i)) {
           x$content[i] <-
               tm_parLapply(x$content[i],
                            function(d) tm_reduce(d, x$lazy$maps))
           x$lazy$index[i] <- FALSE
       }

       # Clean up if everything is materialized
       if (!any(x$lazy$index))
           x["lazy"] <- list(NULL)
    }
    x
}

tm_reduce <-
function(x, tmFuns, ...)
    Reduce(function(f, ...) f(...), tmFuns, x, right = TRUE)

getTransformations <-
function()
    c("removeNumbers", "removePunctuation", "removeWords", "stemDocument",
      "stripWhitespace")

content_transformer <-
function(FUN)
{
    f <- function(x, ...) {
        content(x) <- FUN(content(x), ...)
        x
    }
    class(f) <- c("content_transformer", "function")
    f
}

removeNumbers <-
function(x, ...)
    UseMethod("removeNumbers")
removeNumbers.character <-
function(x, ucp = FALSE, ...)
{
    if (ucp)
        gsub("\\p{Nd}+", "", x, perl = TRUE)
    else
        .Call(`_tm_remove_chars`, x, 1L)
}
removeNumbers.PlainTextDocument <-
    content_transformer(removeNumbers.character)

removePunctuation <-
function(x, ...)
    UseMethod("removePunctuation")
removePunctuation.character <-
function(x,
         preserve_intra_word_contractions = FALSE,
         preserve_intra_word_dashes = FALSE,
         ucp = FALSE,
         ...)
{
    # Assume there are no ASCII 0x01 (SOH) or ASCII 0x02 (STX) characters.
    if (preserve_intra_word_contractions)
        x <- gsub("(\\w)'(\\w)", "\\1\1\\2", x, perl = TRUE)
    if (preserve_intra_word_dashes)
        x <- gsub("(\\w)-(\\w)", "\\1\2\\2", x, perl = TRUE)

    if (ucp)
        x <- gsub("\\p{P}+", "", x, perl = TRUE)
    else
        x <- .Call(`_tm_remove_chars`, x, 0L)

    if (preserve_intra_word_contractions)
        x <- gsub("\1", "'", x, fixed = TRUE)
    if (preserve_intra_word_dashes)
        x <- gsub("\2", "-", x, fixed = TRUE)

    x
}
removePunctuation.PlainTextDocument <-
    content_transformer(removePunctuation.character)

removeWords <-
function(x, words)
    UseMethod("removeWords", x)
# Improvements by Kurt Hornik
removeWords.character <-
function(x, words)
    gsub(sprintf("(*UCP)\\b(%s)\\b",
                 paste(sort(words, decreasing = TRUE), collapse = "|")),
         "", x, perl = TRUE)
removeWords.PlainTextDocument <-
    content_transformer(removeWords.character)

stemDocument <-
function(x, language = "english")
    UseMethod("stemDocument", x)
stemDocument.character <-
function(x, language = "english")
{
    s <- unlist(lapply(x, function(line)
                              paste(SnowballC::wordStem(words(line),
                                                        as.character(language)),
                                    collapse = " ")))
    if (is.character(s)) s else ""
}
stemDocument.PlainTextDocument <-
function(x, language = meta(x, "language"))
{
    language <- as.character(language)
    if (identical(language, "") ||
        identical(language, character(0)) ||
        is.na(language))
        language <- "english"

    content_transformer(stemDocument.character)(x)
}

stripWhitespace <-
function(x)
    UseMethod("stripWhitespace", x)
stripWhitespace.character <-
function(x)
    gsub("[[:space:]]+", " ", x)
stripWhitespace.PlainTextDocument <-
    content_transformer(stripWhitespace.character)
