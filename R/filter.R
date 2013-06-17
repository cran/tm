# Author: Ingo Feinerer
# Filters

tm_filter <- function(x, ..., FUN = searchFullText, doclevel = TRUE, useMeta = FALSE) UseMethod("tm_filter", x)
tm_filter.Corpus <- function(x, ..., FUN = searchFullText, doclevel = TRUE, useMeta = FALSE)
    x[tm_index(x, ..., FUN = FUN, doclevel = doclevel, useMeta = useMeta)]

tm_index <- function(x, ..., FUN = searchFullText, doclevel = TRUE, useMeta = FALSE) UseMethod("tm_index", x)
tm_index.Corpus <- function(x, ..., FUN = searchFullText, doclevel = TRUE, useMeta = FALSE) {
    if (!is.null(attr(FUN, "doclevel")))
        doclevel <- attr(FUN, "doclevel")
    if (doclevel) {
        if (useMeta)
            return(unlist(parallel::mclapply(x, FUN, ..., DMetaData = DMetaData(x))))
        else
            return(unlist(parallel::mclapply(x, FUN, ...)))
    }
    else
        return(FUN(x, ...))
}

getFilters <- function() c("searchFullText", "sFilter", "tm_intersect")

searchFullText <- function(x, pattern) UseMethod("searchFullText", x)
searchFullText.PlainTextDocument <- function(x, pattern) any(grep(pattern, x))
attr(searchFullText, "doclevel") <- TRUE

sFilter <- function(x, s) {
    con <- textConnection(s)
    tokens <- scan(con, "character", quiet = TRUE)
    close(con)
    localMetaNames <- unique(unlist(lapply(x, function(y) names(LocalMetaData(y)))))
    localMetaTokens <- localMetaNames[localMetaNames %in% tokens]
    tags <- c("Author", "DateTimeStamp", "Description", "ID", "Origin", "Heading", "Language")
    query.df <- prescindMeta(x, c(tags, localMetaTokens))
    # Rename to avoid name conflicts
    for (tag in tags)
        names(query.df)[names(query.df) == tag] <- tolower(tag)
    eval(parse(text = s), envir = query.df)
}
attr(sFilter, "doclevel") <- FALSE

tm_intersect <- function(x, y) UseMethod("tm_intersect", x)
tm_intersect.PlainTextDocument <- function(x, y) length(intersect(names(termFreq(x)), y)) > 0
attr(tm_intersect, "doclevel") <- TRUE
