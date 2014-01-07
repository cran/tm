# Author: Ingo Feinerer
# Filters

tm_filter <- function(x, ..., FUN, doclevel = TRUE, useMeta = FALSE) UseMethod("tm_filter", x)
tm_filter.Corpus <- function(x, ..., FUN, doclevel = TRUE, useMeta = FALSE)
    x[tm_index(x, ..., FUN = FUN, doclevel = doclevel, useMeta = useMeta)]

tm_index <- function(x, ..., FUN, doclevel = TRUE, useMeta = FALSE) UseMethod("tm_index", x)
tm_index.Corpus <- function(x, ..., FUN, doclevel = TRUE, useMeta = FALSE) {
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
