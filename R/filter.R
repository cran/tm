# Author: Ingo Feinerer
# Filters

getFilters <- function() c("searchFullText", "sFilter", "tmIntersect")

setGeneric("searchFullText", function(object, pattern, ...) standardGeneric("searchFullText"))
setMethod("searchFullText",
          signature(object = "PlainTextDocument", pattern = "character"),
          function(object, pattern, ...) any(grep(pattern, Content(object))))
attr(searchFullText, "doclevel") <- TRUE

sFilter <- function(object, s, ...) {
    con <- textConnection(s)
    tokens <- scan(con, "character", quiet = TRUE)
    close(con)
    localMetaNames <- unique(names(sapply(object, LocalMetaData)))
    localMetaTokens <- localMetaNames[localMetaNames %in% tokens]
    tags <- c("Author", "DateTimeStamp", "Description", "ID", "Origin", "Heading", "Language")
    query.df <- prescindMeta(object, c(tags, localMetaTokens))
    # Rename to avoid name conflicts
    for (tag in tags)
        names(query.df)[names(query.df) == tag] <- tolower(tag)
    attach(query.df)
    try(result <- rownames(query.df) %in% row.names(query.df[eval(parse(text = s)), ]))
    detach(query.df)
    result
}
attr(sFilter, "doclevel") <- FALSE

setGeneric("tmIntersect", function(object, words, ...) standardGeneric("tmIntersect"))
setMethod("tmIntersect",
          signature(object = "PlainTextDocument", words = "character"),
          function(object, words, ...) {
              length(intersect(names(termFreq(object)), words)) > 0
          })
attr(searchFullText, "doclevel") <- TRUE
