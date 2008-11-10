# Author: Ingo Feinerer
# Filters

getFilters <- function() {
   c("searchFullText", "sFilter", "tmIntersect")
}

setGeneric("searchFullText", function(object, pattern, ...) standardGeneric("searchFullText"))
setMethod("searchFullText",
          signature(object = "PlainTextDocument", pattern = "character"),
          function(object, pattern, ...) {
              any(grep(pattern, Content(object)))
          })
attr(searchFullText, "doclevel") <- TRUE

sFilter <- function(object, s, ...) {
    con <- textConnection(s)
    tokens <- scan(con, "character", quiet = TRUE)
    close(con)
    localMetaNames <- unique(names(sapply(object, LocalMetaData)))
    localMetaTokens <- localMetaNames[localMetaNames %in% tokens]
    n <- names(DMetaData(object))
    tags <- c("Author", "DateTimeStamp", "Description", "ID", "Origin", "Heading", "Language", localMetaTokens)
    query.df <- DMetaData(prescindMeta(object, tags))
    if (DBControl(object)[["useDb"]])
        DMetaData(object) <- DMetaData(object)[, setdiff(n, tags), drop = FALSE]
    # Rename to avoid name conflicts
    names(query.df)[names(query.df) == "Author"] <- "author"
    names(query.df)[names(query.df) == "DateTimeStamp"] <- "datetimestamp"
    names(query.df)[names(query.df) == "Description"] <- "description"
    names(query.df)[names(query.df) == "ID"] <- "identifier"
    names(query.df)[names(query.df) == "Origin"] <- "origin"
    names(query.df)[names(query.df) == "Heading"] <- "heading"
    names(query.df)[names(query.df) == "Language"] <- "language"
    attach(query.df)
    try(result <- rownames(query.df) %in% row.names(query.df[eval(parse(text = s)), ]))
    detach(query.df)
    return(result)
}
attr(sFilter, "doclevel") <- FALSE

setGeneric("tmIntersect", function(object, words, ...) standardGeneric("tmIntersect"))
setMethod("tmIntersect",
          signature(object = "PlainTextDocument", words = "character"),
          function(object, words, ...) {
              length(intersect(names(termFreq(object)), words)) > 0
          })
attr(searchFullText, "doclevel") <- TRUE
