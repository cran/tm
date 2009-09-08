# Author: Ingo Feinerer

# CMetaData = *MetaData* describing only the Document *C*ollection itself
CMetaData <- function(x) UseMethod("CMetaData", x)
CMetaData.Corpus <- function(x) attr(x, "CMetaData")

# Node ID, actual meta data, and possibly other nodes as children
.MetaDataNode <- function(nodeid = 0, meta = list(create_date = as.POSIXlt(Sys.time(), tz = "GMT"), creator = Sys.getenv("LOGNAME")), children = NULL) {
    structure(list(NodeID = nodeid, MetaData = meta, Children = children),
              class = "MetaDataNode")
}
print.MetaDataNode <- function(x, ...) print(x$MetaData)

DMetaData <- function(x) UseMethod("DMetaData", x)
DMetaData.VCorpus <- function(x) attr(x, "DMetaData")
DMetaData.PCorpus <- function(x) {
    db <- filehash::dbInit(DBControl(x)[["dbName"]], DBControl(x)[["dbType"]])
    result <- filehash::dbFetch(db, "DMetaData")
    index <- attr(x, "DMetaData")[[1, "subset"]]
    if (!any(is.na(index)))
        result <- result[index, , drop = FALSE]
    result
}
`DMetaData<-` <- function(x, value) UseMethod("DMetaData<-", x)
`DMetaData<-.PCorpus` <- function(x, value) {
    db <- filehash::dbInit(DBControl(x)[["dbName"]], DBControl(x)[["dbType"]])
    db[["DMetaData"]] <- value
    attr(x, "DMetaData")[[1, "subset"]] <- NA
    x
}
`DMetaData<-.VCorpus` <- function(x, value) {
    attr(x, "DMetaData") <- value
    x
}

meta <- function(x, tag, type = NULL) UseMethod("meta", x)
meta.Corpus <- function(x, tag, type = c("indexed", "corpus", "local")) {
    type <- match.arg(type)
    if (identical(type, "indexed"))
        return(DMetaData(x)[tag])
    if (missing(tag) && identical(type, "corpus"))
        return(CMetaData(x))
    if (identical(type, "corpus"))
        return(CMetaData(x)$MetaData[[tag]])
    if (missing(tag) && identical(type, "local"))
        return(invisible(lapply(x, meta)))
    if (identical(type, "local"))
        return(lapply(x, meta, tag))
}
meta.TextDocument <- function(x, tag, type = NULL) {
    if (missing(tag)) {
        attrs <- sort(setdiff(names(attributes(x)), c("class", "LocalMetaData")))
        cat("Available meta data pairs are:\n")
        for (a in attrs)
            cat(sprintf("  %-13s: %s\n", a, paste(as.character(attr(x, a)), collapse = " ")))
        if (length(LocalMetaData(x)) > 0) {
            cat("User-defined local meta data pairs are:\n")
            print(LocalMetaData(x))
        }
    }
    else {
        if (tag %in% names(attributes(x))) attr(x, tag)
        else LocalMetaData(x)[[tag]]
    }
}
meta.TextRepository <- function(x, tag, type = NULL) {
    if (missing(tag))
        RepoMetaData(x)
    else
        RepoMetaData(x)[[tag]]
}

`meta<-` <- function(x, tag, type = NULL, value) UseMethod("meta<-", x)
`meta<-.Corpus` <- function(x, tag, type = c("indexed", "corpus", "local"), value) {
    type <- match.arg(type)
    if ((type != "indexed") && (type != "corpus") && (type != "local"))
        stop("invalid type")
    if (identical(type, "indexed"))
        DMetaData(x)[, tag] <- value
    else if (identical(type, "local"))
        for (i in seq_along(x))
            meta(x[[i]], tag) <- value[[i]]
    else # (type == "corpus")
        attr(x, "CMetaData")$MetaData[[tag]] <- value
    x
}
`meta<-.TextDocument` <- function(x, tag, type = NULL, value) {
    if (tag %in% setdiff(names(attributes(x)), ".Data"))
        attr(x, tag) <- value
    else
        attr(x, "LocalMetaData")[[tag]] <- value
    x
}
`meta<-.TextRepository` <- function(x, tag, type = NULL, value) {
    attr(x, "RepoMetaData")[[tag]] <- value
    x
}

# Simple Dublin Core to tm meta data mappings
# http://en.wikipedia.org/wiki/Dublin_core#Simple_Dublin_Core
Dublin_Core_tm <-
function(DCElem = c("Title", "Creator", "Description", "Date", "Identifier", "Language", "Subject",
         "Publisher", "Contributor", "Type", "Format", "Source", "Relation", "Coverage", "Rights"))
{
    DCElem <- match.arg(DCElem)
    if (identical(DCElem, "Title")) return(list(tag = "Heading", type = "local"))
    if (identical(DCElem, "Creator")) return(list(tag = "Author", type = "local"))
    if (identical(DCElem, "Description")) return(list(tag = "Description", type = "local"))
    if (identical(DCElem, "Date")) return(list(tag = "DateTimeStamp", type = "local"))
    if (identical(DCElem, "Identifier")) return(list(tag = "ID", type = "local"))
    if (identical(DCElem, "Language")) return(list(tag = "Language", type = "local"))
    # Source -> Origin ?

    if (identical(DCElem, "Subject") || identical(DCElem, "Publisher") || identical(DCElem, "Contributor") ||
        identical(DCElem, "Type") || identical(DCElem, "Format") || identical(DCElem, "Source") ||
        identical(DCElem, "Relation") || identical(DCElem, "Coverage") || identical(DCElem, "Rights"))
        return(list(tag = DCElem, type = "extended"))

    stop("invalid simple Dublin Core meta data element")
}

DublinCore <- function(x, tag = NULL) {
    if (is.null(tag)) {
        elements <- c("Title", "Creator", "Subject", "Description", "Publisher",
                      "Contributor", "Date", "Type", "Format", "Identifier",
                      "Source", "Language", "Relation", "Coverage", "Rights")
        cat("Simple Dublin Core meta data pairs are:\n")
        for (e in elements) {
            DCtm <- Dublin_Core_tm(e)
            DCvalue <- meta(x, DCtm$tag)
            cat(sprintf("  %-11s: %s\n", e, paste(as.character(DCvalue), collapse = " ")))
        }
    }
    else {
        DCtm <- Dublin_Core_tm(tag)
        meta(x, DCtm$tag)
    }
}

`DublinCore<-` <- function(x, tag, value) {
    DCtm <- Dublin_Core_tm(tag)
    meta(x, DCtm$tag) <- value
    x
}

prescindMeta <- function(x, meta) {
    df <- DMetaData(x)

    for (m in meta)
        df <- cbind(df, structure(data.frame(I(meta(x, tag = m, type = "local"))), names = m))

    df
}
