# Author: Ingo Feinerer

# The "..." are additional arguments for the FunctionGenerator parser
setGeneric("TextDocCol", function(object, parser = readPlain, load = FALSE, ...) standardGeneric("TextDocCol"))
setMethod("TextDocCol",
          signature(object = "Source"),
          function(object, parser = readPlain, load = FALSE, ...) {
              if (inherits(parser, "FunctionGenerator"))
                  parser <- parser(...)

              tdl <- list()
              counter <- 1
              while (!eoi(object)) {
                  object <- stepNext(object)
                  elem <- getElem(object)
                  # If there is no Load on Demand support
                  # we need to load the corpus into memory at startup
                  if (!object@LoDSupport)
                      load <- TRUE
                  tdl <- c(tdl, list(parser(elem, load, as.character(counter))))
                  counter <- counter + 1
              }

              dmeta.df <- data.frame(MetaID = rep(0, length(tdl)), stringsAsFactors = FALSE)
              cmeta.node <- new("MetaDataNode",
                            NodeID = 0,
                            MetaData = list(create_date = Sys.time(), creator = Sys.getenv("LOGNAME")),
                            children = list())

              return(new("TextDocCol", .Data = tdl, DMetaData = dmeta.df, CMetaData = cmeta.node))
          })

setGeneric("loadDoc", function(object, ...) standardGeneric("loadDoc"))
setMethod("loadDoc",
          signature(object = "PlainTextDocument"),
          function(object, ...) {
              if (!Cached(object)) {
                  con <- eval(URI(object))
                  corpus <- readLines(con)
                  close(con)
                  Corpus(object) <- corpus
                  Cached(object) <- TRUE
                  return(object)
              } else {
                  return(object)
              }
          })
setMethod("loadDoc",
          signature(object =  "XMLTextDocument"),
          function(object, ...) {
              if (!Cached(object)) {
                  con <- eval(URI(object))
                  corpus <- paste(readLines(con), "\n", collapse = "")
                  close(con)
                  doc <- xmlTreeParse(corpus, asText = TRUE)
                  class(doc) <- "list"
                  Corpus(object) <- doc
                  Cached(object) <- TRUE
                  return(object)
              } else {
                  return(object)
              }
          })
setMethod("loadDoc",
          signature(object = "NewsgroupDocument"),
          function(object, ...) {
              if (!Cached(object)) {
                  con <- eval(URI(object))
                  mail <- readLines(con)
                  close(con)
                  Cached(object) <- TRUE
                  for (index in seq(along = mail)) {
                      if (mail[index] == "")
                          break
                  }
                  Corpus(object) <- mail[(index + 1):length(mail)]
                  return(object)
              } else {
                  return(object)
              }
          })

setGeneric("tmUpdate", function(object, origin, parser = readPlain, ...) standardGeneric("tmUpdate"))
# Update is only supported for directories
# At the moment no other LoD devices are available anyway
setMethod("tmUpdate",
          signature(object = "TextDocCol", origin = "DirSource"),
          function(object, origin, parser = readPlain, load = FALSE, ...) {
              if (inherits(parser, "FunctionGenerator"))
                  parser <- parser(...)

              object.filelist <- unlist(lapply(object, function(x) {as.character(URI(x))[2]}))
              new.files <- setdiff(origin@FileList, object.filelist)

              for (filename in new.files) {
                  elem <- list(content = readLines(filename),
                               uri = substitute(file(filename)))
                  object <- appendElem(object, parser(elem, load, filename))
              }

              return(object)
          })

setGeneric("tmMap", function(object, FUN, ...) standardGeneric("tmMap"))
setMethod("tmMap",
          signature(object = "TextDocCol", FUN = "function"),
          function(object, FUN, ...) {
              result <- object
              # Note that text corpora are automatically loaded into memory via \code{[[}
              result@.Data <- lapply(object, FUN, ..., DMetaData = DMetaData(object))
              return(result)
          })

setGeneric("asPlain", function(object, FUN, ...) standardGeneric("asPlain"))
setMethod("asPlain",
          signature(object = "PlainTextDocument"),
          function(object, FUN, ...) {
              return(object)
          })
setMethod("asPlain",
          signature(object = "XMLTextDocument", FUN = "function"),
          function(object, FUN, ...) {
              corpus <- Corpus(object)

              # As XMLDocument is no native S4 class, restore valid information
              class(corpus) <- "XMLDocument"
              names(corpus) <- c("doc","dtd")

              return(FUN(xmlRoot(corpus), ...))
          })

setGeneric("tmTolower", function(object, ...) standardGeneric("tmTolower"))
setMethod("tmTolower",
          signature(object = "PlainTextDocument"),
          function(object, ...) {
              Corpus(object) <- tolower(object)
              return(object)
          })

setGeneric("stripWhitespace", function(object, ...) standardGeneric("stripWhitespace"))
setMethod("stripWhitespace",
          signature(object = "PlainTextDocument"),
          function(object, ...) {
              Corpus(object) <- gsub("[[:space:]]+", " ", object)
              return(object)
          })

setGeneric("stemDoc", function(object, ...) standardGeneric("stemDoc"))
setMethod("stemDoc",
          signature(object = "PlainTextDocument"),
          function(object, ...) {
              require("Rstem")
              splittedCorpus <- unlist(strsplit(object, " ", fixed = TRUE))
              stemmedCorpus <- Rstem::wordStem(splittedCorpus)
              Corpus(object) <- paste(stemmedCorpus, collapse = " ")
              return(object)
          })

setGeneric("removeWords", function(object, stopwords, ...) standardGeneric("removeWords"))
setMethod("removeWords",
          signature(object = "PlainTextDocument", stopwords = "character"),
          function(object, stopwords, ...) {
              require("Rstem")
              splittedCorpus <- unlist(strsplit(object, " ", fixed = TRUE))
              noStopwordsCorpus <- splittedCorpus[!splittedCorpus %in% stopwords]
              Corpus(object) <- paste(noStopwordsCorpus, collapse = " ")
              return(object)
          })

setGeneric("tmFilter", function(object, ..., FUN = sFilter, doclevel = FALSE) standardGeneric("tmFilter"))
setMethod("tmFilter",
          signature(object = "TextDocCol"),
          function(object, ..., FUN = sFilter, doclevel = FALSE) {
              if (doclevel)
                  return(object[sapply(object, FUN, ..., DMetaData = DMetaData(object))])
              else
                  return(object[FUN(object, ...)])
          })

setGeneric("tmIndex", function(object, ..., FUN = sFilter, doclevel = FALSE) standardGeneric("tmIndex"))
setMethod("tmIndex",
          signature(object = "TextDocCol"),
          function(object, ..., FUN = sFilter, doclevel = FALSE) {
              if (doclevel)
                  return(sapply(object, FUN, ..., DMetaData = DMetaData(object)))
              else
                  return(FUN(object, ...))
          })

sFilter <- function(object, s, ...) {
    query.df <- DMetaData(object)
    con <- textConnection(s)
    tokens <- scan(con, "character")
    close(con)
    local.meta <- lapply(object, LocalMetaData)
    local.used.meta <- lapply(local.meta, function(x) names(x) %in% tokens)
    l.meta <- NULL
    for (i in 1:length(object)) {
        l.meta <- c(l.meta, list(local.meta[[i]][local.used.meta[[i]]]))
    }
    # Load local meta data from text documents into data frame
    for (i in 1:length(l.meta)) {
        l.meta[[i]] <- c(l.meta[[i]], list(author = Author(object[[i]])))
        l.meta[[i]] <- c(l.meta[[i]], list(datetimestamp = DateTimeStamp(object[[i]])))
        l.meta[[i]] <- c(l.meta[[i]], list(description = Description(object[[i]])))
        l.meta[[i]] <- c(l.meta[[i]], list(identifier = ID(object[[i]])))
        l.meta[[i]] <- c(l.meta[[i]], list(origin = Origin(object[[i]])))
        l.meta[[i]] <- c(l.meta[[i]], list(heading = Heading(object[[i]])))
    }
    for (i in 1:length(l.meta)) {
        for (j in 1:length(l.meta[[i]])) {
            m <- l.meta[[i]][[j]]
            m.name <- names(l.meta[[i]][j])
            if (!(m.name %in% names(query.df))) {
                before <- rep(NA, i - 1)
                after <- rep(NA, length(l.meta) - i)
                if (length(m) > 1) {
                    nl <- vector("list", length(l.meta))
                    nl[1:(i-1)] <- before
                    nl[i] <- list(m)
                    nl[(i+1):length(l.meta)] <- after
                    insert <- data.frame(I(nl), stringsAsFactors = FALSE)
                }
                else
                    insert <- c(before, m, after)
                query.df <- cbind(query.df, insert, stringsAsFactors = FALSE)
                names(query.df)[length(query.df)] <- m.name
            }
            else {
                if (is.null(m))
                    m <- NA
                if (length(m) > 1) {
                    rl <- query.df[ , m.name]
                    rl[i] <- list(m)
                    query.df[ , m.name] <- data.frame(I(rl), stringsAsFactors = FALSE)
                }
                else
                    query.df[i, m.name] <- m
            }
        }
    }
    attach(query.df)
    try(result <- rownames(query.df) %in% row.names(query.df[eval(parse(text = s)), ]))
    detach(query.df)
    return(result)
}

setGeneric("searchFullText", function(object, pattern, ...) standardGeneric("searchFullText"))
setMethod("searchFullText",
          signature(object = "PlainTextDocument", pattern = "character"),
          function(object, pattern, ...) {
              return(any(grep(pattern, Corpus(object))))
          })

setGeneric("appendElem", function(object, data, meta = NULL) standardGeneric("appendElem"))
setMethod("appendElem",
          signature(object = "TextDocCol", data = "TextDocument"),
          function(object, data, meta = NULL) {
              object@.Data[[length(object)+1]] <- data
              object@DMetaData <- rbind(object@DMetaData, c(MetaID = CMetaData(object)@NodeID, meta))
              return(object)
          })

setGeneric("appendMeta", function(object, cmeta = NULL, dmeta = NULL) standardGeneric("appendMeta"))
setMethod("appendMeta",
          signature(object = "TextDocCol"),
          function(object, cmeta = NULL, dmeta = NULL) {
              object@CMetaData@MetaData <- c(object@CMetaData@MetaData, cmeta)
              if (!is.null(cmeta))
                  object@DMetaData <- cbind(object@DMetaData, dmeta)
              return(object)
          })

setGeneric("removeMeta", function(object, cname = NULL, dname = NULL) standardGeneric("removeMeta"))
setMethod("removeMeta",
          signature(object = "TextDocCol"),
          function(object, cname = NULL, dname = NULL) {
              if (!is.null(cname)) {
                  object@CMetaData@MetaData <- CMetaData(object)@MetaData[names(CMetaData(object)@MetaData) != cname]
              }
              if (!is.null(dname)) {
                  object@DMetaData <- DMetaData(object)[names(DMetaData(object)) != dname]
              }
              return(object)
          })

setGeneric("prescindMeta", function(object, meta) standardGeneric("prescindMeta"))
setMethod("prescindMeta",
          signature(object = "TextDocCol", meta = "character"),
          function(object, meta) {
              for (m in meta) {
                  if (m %in% c("Author", "DateTimeStamp", "Description", "ID", "Origin", "Heading")) {
                      local.m <- lapply(object, m)
                      local.m <- lapply(local.m, function(x) if (is.null(x)) return(NA) else return(x))
                      local.m <- unlist(local.m)
                      object@DMetaData <- cbind(DMetaData(object), data.frame(m = local.m), stringsAsFactors = FALSE)
                      names(object@DMetaData)[length(object@DMetaData)] <- m
                  }
                  else {
                      local.meta <- lapply(object, LocalMetaData)
                      local.m <- lapply(local.meta, "[[", m)
                      local.m <- lapply(local.m, function(x) if (is.null(x)) return(NA) else return(x))
                      if (length(local.m) == length(unlist(local.m)))
                          local.m <- unlist(local.m)
                      else
                          local.m <- I(local.m)
                      object@DMetaData <- cbind(DMetaData(object), data.frame(m = local.m), stringsAsFactors = FALSE)
                      names(object@DMetaData)[length(object@DMetaData)] <- m
                  }
              }
              return(object)
          })

setMethod("[",
          signature(x = "TextDocCol", i = "ANY", j = "ANY", drop = "ANY"),
          function(x, i, j, ... , drop) {
              if(missing(i))
                  return(x)

              object <- x
              object@.Data <- x@.Data[i, ..., drop = FALSE]
              df <- as.data.frame(DMetaData(object)[i, ])
              names(df) <- names(DMetaData(object))
              object@DMetaData <- df
              return(object)
          })

setMethod("[<-",
          signature(x = "TextDocCol", i = "ANY", j = "ANY", value = "ANY"),
          function(x, i, j, ... , value) {
              object <- x
              object@.Data[i, ...] <- value
              return(object)
          })

setMethod("[[",
          signature(x = "TextDocCol", i = "ANY", j = "ANY"),
          function(x, i, j, ...) {
              return(loadDoc(x@.Data[[i]]))
          })

setMethod("[[<-",
          signature(x = "TextDocCol", i = "ANY", j = "ANY", value = "ANY"),
          function(x, i, j, ..., value) {
              object <- x
              object@.Data[[i, ...]] <- value
              return(object)
          })

# Update \code{NodeID}s of a CMetaData tree
update_id <- function(object, id = 0, mapping = NULL, left.mapping = NULL, level = 0) {
    # Traversal of (binary) CMetaData tree with setup of \code{NodeID}s
    set_id <- function(object) {
        object@NodeID <- id
        id <<- id + 1
        level <<- level + 1

        if (length(object@children) > 0) {
            mapping <<- cbind(mapping, c(object@children[[1]]@NodeID, id))
            left <- set_id(object@children[[1]])
            if (level == 1) {
                left.mapping <<- mapping
                mapping <<- NULL
            }
            mapping <<- cbind(mapping, c(object@children[[2]]@NodeID, id))
            right <- set_id(object@children[[2]])

            object@children <- list(left, right)
        }
        level <<- level - 1

        return(object)
    }

    return(list(root = set_id(object), left.mapping = left.mapping, right.mapping = mapping))
}

setMethod("c",
          signature(x = "TextDocCol"),
          function(x, ..., meta = list(merge_date = Sys.time(), merger = Sys.getenv("LOGNAME")), recursive = TRUE) {
              args <- list(...)
              if(length(args) == 0)
                  return(x)

              result <- x
              for (c in args) {
                  if (!inherits(c, "TextDocCol"))
                      stop("invalid argument")
                  result <- c2(result, c)
              }
              return(result)
          })

setGeneric("c2", function(x, y, ..., meta = list(merge_date = Sys.time(), merger = Sys.getenv("LOGNAME")), recursive = TRUE) standardGeneric("c2"))
setMethod("c2",
          signature(x = "TextDocCol", y = "TextDocCol"),
          function(x, y, ..., meta = list(merge_date = Sys.time(), merger = Sys.getenv("LOGNAME")), recursive = TRUE) {
              object <- x
              # Concatenate data slots
              object@.Data <- c(as(x, "list"), as(y, "list"))

              # Update the CMetaData tree
              cmeta <- new("MetaDataNode", NodeID = 0, MetaData = meta, children = list(CMetaData(x), CMetaData(y)))
              update.struct <- update_id(cmeta)
              object@CMetaData <- update.struct$root

              # Find indices to be updated for the left tree
              indices.mapping <- NULL
              for (m in levels(as.factor(DMetaData(x)$MetaID))) {
                  indices <- (DMetaData(x)$MetaID == m)
                  indices.mapping <- c(indices.mapping, list(m = indices))
                  names(indices.mapping)[length(indices.mapping)] <- m
              }

              # Update the DMetaData data frames for the left tree
              for (i in 1:ncol(update.struct$left.mapping)) {
                  map <- update.struct$left.mapping[,i]
                  x@DMetaData$MetaID <- replace(DMetaData(x)$MetaID, indices.mapping[[as.character(map[1])]], map[2])
              }

              # Find indices to be updated for the right tree
              indices.mapping <- NULL
              for (m in levels(as.factor(DMetaData(y)$MetaID))) {
                  indices <- (DMetaData(y)$MetaID == m)
                  indices.mapping <- c(indices.mapping, list(m = indices))
                  names(indices.mapping)[length(indices.mapping)] <- m
              }

              # Update the DMetaData data frames for the right tree
              for (i in 1:ncol(update.struct$right.mapping)) {
                  map <- update.struct$right.mapping[,i]
                  y@DMetaData$MetaID <- replace(DMetaData(y)$MetaID, indices.mapping[[as.character(map[1])]], map[2])
              }

              # Merge the DMetaData data frames
              labels <- setdiff(names(DMetaData(y)), names(DMetaData(x)))
              na.matrix <- matrix(NA, nrow = nrow(DMetaData(x)), ncol = length(labels), dimnames = list(row.names(DMetaData(x)), labels))
              x.dmeta.aug <- cbind(DMetaData(x), na.matrix)
              labels <- setdiff(names(DMetaData(x)), names(DMetaData(y)))
              na.matrix <- matrix(NA, nrow = nrow(DMetaData(y)), ncol = length(labels), dimnames = list(row.names(DMetaData(y)), labels))
              y.dmeta.aug <- cbind(DMetaData(y), na.matrix)
              object@DMetaData <- rbind(x.dmeta.aug, y.dmeta.aug)

              return(object)
          })


setMethod("c",
          signature(x = "TextDocument"),
          function(x, ..., recursive = TRUE){
              args <- list(...)
              if(length(args) == 0)
                  return(x)

              dmeta.df <- data.frame(MetaID = rep(0, length(list(x, ...))), stringsAsFactors = FALSE)
              cmeta.node <- new("MetaDataNode",
                            NodeID = 0,
                            MetaData = list(create_date = Sys.time(), creator = Sys.getenv("LOGNAME")),
                            children = list())

              return(new("TextDocCol", .Data = list(x, ...), DMetaData = dmeta.df, CMetaData = cmeta.node))
          })

setMethod("length",
          signature(x = "TextDocCol"),
          function(x){
              return(length(as(x, "list")))
    })

setMethod("show",
          signature(object = "TextDocCol"),
          function(object){
              cat(sprintf(ngettext(length(object),
                                   "A text document collection with %d text document\n",
                                   "A text document collection with %d text documents\n"),
                          length(object)))
    })

setMethod("summary",
          signature(object = "TextDocCol"),
          function(object){
              show(object)
              if (length(DMetaData(object)) > 0) {
                  cat(sprintf(ngettext(length(CMetaData(object)@MetaData),
                                              "\nThe metadata consists of %d tag-value pair and a data frame\n",
                                              "\nThe metadata consists of %d tag-value pairs and a data frame\n"),
                                       length(CMetaData(object)@MetaData)))
                  cat("Available tags are:\n")
                  cat(names(CMetaData(object)@MetaData), "\n")
                  cat("Available variables in the data frame are:\n")
                  cat(names(DMetaData(object)), "\n")
              }
    })

setGeneric("inspect", function(object) standardGeneric("inspect"))
setMethod("inspect",
          signature("TextDocCol"),
          function(object) {
              summary(object)
              cat("\n")
              show(object@.Data)
          })

# No metadata is checked
setGeneric("%IN%", function(x, y) standardGeneric("%IN%"))
setMethod("%IN%",
          signature(x = "TextDocument", y = "TextDocCol"),
          function(x, y) {
              x %in% y
          })
