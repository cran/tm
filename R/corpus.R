# Author: Ingo Feinerer

prepareReader <- function(readerControl, defaultReader = NULL, ...) {
    if (is.null(readerControl$reader))
        readerControl$reader <- defaultReader
    if (is(readerControl$reader, "FunctionGenerator"))
        readerControl$reader <- readerControl$reader(...)
    if (is.null(readerControl$language))
        readerControl$language <- "eng"
    readerControl
}

## Fast Corpus
##   - provides a prototype implementation of a more time and memory efficient representation of a corpus
##   - allows performance tests and comparisons to other corpus types
#FCorpus <- function(object, readerControl = list(language = "eng")) {
#    readerControl <- prepareReader(readerControl)
#
#    if (!object@Vectorized)
#        stop("Source is not vectorized")
#
#    tdl <- lapply(mapply(c, pGetElem(object), id = seq_len(object@Length), SIMPLIFY = FALSE),
#                  function(x) readSlim(x[c("content", "uri")],
#                                       readerControl$language,
#                                       as.character(x$id)))
#
#    new("FCorpus", .Data = tdl)
#}

PCorpus <- function(object,
                    readerControl = list(reader = object@DefaultReader, language = "eng"),
                    dbControl = list(dbName = "", dbType = "DB1"),
                    ...) {
    readerControl <- prepareReader(readerControl, object@DefaultReader, ...)

    if (!filehash::dbCreate(dbControl$dbName, dbControl$dbType))
        stop("error in creating database")
    db <- filehash::dbInit(dbControl$dbName, dbControl$dbType)

    # Allocate memory in advance if length is known
    tdl <- if (object@Length > 0)
        vector("list", as.integer(object@Length))
    else
        list()

    counter <- 1
    while (!eoi(object)) {
        object <- stepNext(object)
        elem <- getElem(object)
        doc <- readerControl$reader(elem, readerControl$language, as.character(counter))
        filehash::dbInsert(db, ID(doc), doc)
        if (object@Length > 0) tdl[[counter]] <- ID(doc)
        else tdl <- c(tdl, ID(doc))
        counter <- counter + 1
    }

    df <- data.frame(MetaID = rep(0, length(tdl)), stringsAsFactors = FALSE)
    filehash::dbInsert(db, "DMetaData", df)
    dmeta.df <- data.frame(key = "DMetaData", subset = I(list(NA)))

    cmeta.node <- new("MetaDataNode",
                      NodeID = 0,
                      MetaData = list(create_date = as.POSIXlt(Sys.time(), tz = "GMT"), creator = Sys.getenv("LOGNAME")),
                      children = list())

    new("PCorpus", .Data = tdl, DMetaData = dmeta.df, CMetaData = cmeta.node, DBControl = dbControl)
}

# The "..." are additional arguments for the FunctionGenerator reader
VCorpus <- Corpus <- function(object,
                    readerControl = list(reader = object@DefaultReader, language = "eng"),
                    ...) {
    readerControl <- prepareReader(readerControl, object@DefaultReader, ...)

    # Allocate memory in advance if length is known
    tdl <- if (object@Length > 0)
        vector("list", as.integer(object@Length))
    else
        list()

    if (object@Vectorized)
        tdl <- lapply(mapply(c, pGetElem(object), id = seq_len(object@Length), SIMPLIFY = FALSE),
                      function(x) readerControl$reader(x[c("content", "uri")],
                                                       readerControl$language,
                                                       as.character(x$id)))
    else {
        counter <- 1
        while (!eoi(object)) {
            object <- stepNext(object)
            elem <- getElem(object)
            doc <- readerControl$reader(elem, readerControl$language, as.character(counter))
            if (object@Length > 0)
                tdl[[counter]] <- doc
            else
                tdl <- c(tdl, list(doc))
            counter <- counter + 1
        }
    }

    df <- data.frame(MetaID = rep(0, length(tdl)), stringsAsFactors = FALSE)
    cmeta.node <- new("MetaDataNode",
                      NodeID = 0,
                      MetaData = list(create_date = as.POSIXlt(Sys.time(), tz = "GMT"), creator = Sys.getenv("LOGNAME")),
                      children = list())

    new("VCorpus", .Data = tdl, DMetaData = df, CMetaData = cmeta.node)
}

setGeneric("tmMap", function(object, FUN, ..., lazy = FALSE) standardGeneric("tmMap"))
#setMethod("tmMap",
#          signature(object = "FCorpus", FUN = "function"),
#          function(object, FUN, ..., lazy = FALSE) {
#              if (lazy)
#                  warning("lazy mapping is deactivated")
#
#              new("FCorpus", .Data = lapply(object, FUN, ..., DMetaData = data.frame()))
#          })
setMethod("tmMap",
          signature(object = "VCorpus", FUN = "function"),
          function(object, FUN, ..., lazy = FALSE) {
              result <- object
              # Lazy mapping
              if (lazy) {
                  lazyTmMap <- meta(object, tag = "lazyTmMap", type = "corpus")
                  if (is.null(lazyTmMap)) {
                      meta(result, tag = "lazyTmMap", type = "corpus") <-
                          list(index = rep(TRUE, length(result)),
                               maps = list(function(x, DMetaData) FUN(x, ..., DMetaData = DMetaData)))
                  }
                  else {
                      lazyTmMap$maps <- c(lazyTmMap$maps, list(function(x, DMetaData) FUN(x, ..., DMetaData = DMetaData)))
                      meta(result, tag = "lazyTmMap", type = "corpus") <- lazyTmMap
                  }
              }
              else {
                  result@.Data <- if (clusterAvailable())
                      snow::parLapply(snow::getMPIcluster(), object, FUN, ..., DMetaData = DMetaData(object))
                  else
                      lapply(object, FUN, ..., DMetaData = DMetaData(object))
              }
              result
          })
setMethod("tmMap",
          signature(object = "PCorpus", FUN = "function"),
          function(object, FUN, ..., lazy = FALSE) {
              if (lazy)
                  warning("lazy mapping is deactived when using database backend")
              db <- filehash::dbInit(DBControl(object)[["dbName"]], DBControl(object)[["dbType"]])
              i <- 1
              for (id in unlist(object)) {
                  db[[id]] <- FUN(object[[i]], ..., DMetaData = DMetaData(object))
                  i <- i + 1
              }
              # Suggested by Christian Buchta
              filehash::dbReorganize(db)

              object
          })

# Materialize lazy mappings
# Improvements by Christian Buchta
materialize <- function(corpus, range = seq_along(corpus)) {
    lazyTmMap <- meta(corpus, tag = "lazyTmMap", type = "corpus")
    if (!is.null(lazyTmMap)) {
       # Make valid and lazy index
       idx <- (seq_along(corpus) %in% range) & lazyTmMap$index
       if (any(idx)) {
           res <- corpus@.Data[idx]
           for (m in lazyTmMap$maps)
               res <- lapply(res, m, DMetaData = DMetaData(corpus))
           corpus@.Data[idx] <- res
           lazyTmMap$index[idx] <- FALSE
       }
    }
    # Clean up if everything is materialized
    if (!any(lazyTmMap$index))
        lazyTmMap <- NULL
    meta(corpus, tag = "lazyTmMap", type = "corpus") <- lazyTmMap
    corpus
}

setGeneric("asPlain", function(object, FUN, ...) standardGeneric("asPlain"))
setMethod("asPlain", signature(object = "PlainTextDocument"),
          function(object, FUN, ...) object)
setMethod("asPlain",
          signature(object = "XMLTextDocument"),
          function(object, FUN, ...) {
              require("XML")

              corpus <- Content(object)

              # As XMLDocument is no native S4 class, restore valid information
              class(corpus) <- "XMLDocument"
              names(corpus) <- c("doc","dtd")

              return(FUN(xmlRoot(corpus), ...))
          })
setMethod("asPlain",
          signature(object = "Reuters21578Document"),
          function(object, FUN, ...) {
              require("XML")

              FUN <- convertReut21578XMLPlain
              corpus <- Content(object)

              # As XMLDocument is no native S4 class, restore valid information
              class(corpus) <- "XMLDocument"
              names(corpus) <- c("doc","dtd")

              return(FUN(xmlRoot(corpus), ...))
          })
setMethod("asPlain", signature(object = "RCV1Document"),
          function(object, FUN, ...) convertRCV1Plain(object, ...))
setMethod("asPlain",
          signature(object = "NewsgroupDocument"),
          function(object, FUN, ...) {
              new("PlainTextDocument", .Data = Content(object), Author = Author(object),
                  DateTimeStamp = DateTimeStamp(object), Description = Description(object), ID = ID(object),
                  Origin = Origin(object), Heading = Heading(object), Language = Language(object),
                  LocalMetaData = LocalMetaData(object))
          })
setMethod("asPlain",
          signature(object = "StructuredTextDocument"),
          function(object, FUN, ...) {
              new("PlainTextDocument", .Data = unlist(Content(object)),
                  Author = Author(object), DateTimeStamp = DateTimeStamp(object),
                  Description = Description(object), ID = ID(object), Origin = Origin(object),
                  Heading = Heading(object), Language = Language(object),
                  LocalMetaData = LocalMetaData(object))
          })

setGeneric("tmFilter", function(object, ..., FUN = searchFullText, doclevel = TRUE) standardGeneric("tmFilter"))
setMethod("tmFilter", signature(object = "Corpus"),
          function(object, ..., FUN = searchFullText, doclevel = TRUE)
              object[tmIndex(object, ..., FUN = FUN, doclevel = doclevel)])

setGeneric("tmIndex", function(object, ..., FUN = searchFullText, doclevel = TRUE) standardGeneric("tmIndex"))
setMethod("tmIndex",
          signature(object = "Corpus"),
          function(object, ..., FUN = searchFullText, doclevel = TRUE) {
              if (!is.null(attr(FUN, "doclevel")))
                  doclevel <- attr(FUN, "doclevel")
              if (doclevel) {
                  if (clusterAvailable())
                      return(snow::parSapply(snow::getMPIcluster(), object, FUN, ..., DMetaData = DMetaData(object)))
                  else
                      return(sapply(object, FUN, ..., DMetaData = DMetaData(object)))
              }
              else
                  return(FUN(object, ...))
          })

setGeneric("appendElem", function(object, data, meta = NULL) standardGeneric("appendElem"))
setMethod("appendElem",
          signature(object = "Corpus", data = "TextDocument"),
          function(object, data, meta = NULL) {
              if (DBControl(object)[["useDb"]] && require("filehash")) {
                  db <- dbInit(DBControl(object)[["dbName"]], DBControl(object)[["dbType"]])
                  if (dbExists(db, ID(data)))
                      warning("document with identical ID already exists")
                  dbInsert(db, ID(data), data)
                  object@.Data[[length(object)+1]] <- ID(data)
              }
              else
                  object@.Data[[length(object)+1]] <- data
              DMetaData(object) <- rbind(DMetaData(object), c(MetaID = CMetaData(object)@NodeID, meta))
              return(object)
          })

prescindMeta <- function(object, meta) {
    df <- DMetaData(object)

    for (m in meta)
        df <- cbind(df, structure(data.frame(I(meta(object, tag = m, type = "local"))), names = m))

    df
}

#setMethod("[",
#          signature(x = "FCorpus", i = "ANY", j = "ANY", drop = "ANY"),
#          function(x, i, j, ... , drop) {
#              if (missing(i)) return(x)
#
#              x@.Data <- x@.Data[i, ..., drop = FALSE]
#              x
#          })
setMethod("[",
          signature(x = "PCorpus", i = "ANY", j = "ANY", drop = "ANY"),
          function(x, i, j, ... , drop) {
              if (missing(i)) return(x)

              x@.Data <- x@.Data[i, ..., drop = FALSE]
              index <- x@DMetaData[[1 , "subset"]]
              if (any(is.na(index))) x@DMetaData[[1 , "subset"]] <- i
              else x@DMetaData[[1 , "subset"]] <- index[i]
              x
          })
setMethod("[",
          signature(x = "VCorpus", i = "ANY", j = "ANY", drop = "ANY"),
          function(x, i, j, ... , drop) {
              if (missing(i)) return(x)

              x@.Data <- x@.Data[i, ..., drop = FALSE]
              DMetaData(x) <- DMetaData(x)[i, , drop = FALSE]
              x
          })

setMethod("[<-",
          signature(x = "PCorpus", i = "ANY", j = "ANY", value = "ANY"),
          function(x, i, j, ... , value) {
              db <- filehash::dbInit(DBControl(x)[["dbName"]], DBControl(x)[["dbType"]])
              counter <- 1
              for (id in x@.Data[i, ...]) {
                  if (identical(length(value), 1)) db[[id]] <- value
                  else db[[id]] <- value[[counter]]
                  counter <- counter + 1
              }
              x
          })
setMethod("[<-",
          signature(x = "VCorpus", i = "ANY", j = "ANY", value = "ANY"),
          function(x, i, j, ... , value) {
              x@.Data[i, ...] <- value
              x
          })

setMethod("[[",
          signature(x = "PCorpus", i = "ANY", j = "ANY"),
          function(x, i, j, ...) {
              db <- filehash::dbInit(DBControl(x)[["dbName"]], DBControl(x)[["dbType"]])
              filehash::dbFetch(db, x@.Data[[i]])
          })
setMethod("[[",
          signature(x = "VCorpus", i = "ANY", j = "ANY"),
          function(x, i, j, ...) {
              lazyTmMap <- meta(x, tag = "lazyTmMap", type = "corpus")
              if (!is.null(lazyTmMap))
                  .Call("copyCorpus", x, materialize(x, i))
              x@.Data[[i]]
          })

setMethod("[[<-",
          signature(x = "PCorpus", i = "ANY", j = "ANY", value = "ANY"),
          function(x, i, j, ..., value) {
              db <- filehash::dbInit(DBControl(x)[["dbName"]], DBControl(x)[["dbType"]])
              index <- x@.Data[[i]]
              db[[index]] <- value
              x
          })
setMethod("[[<-",
          signature(x = "VCorpus", i = "ANY", j = "ANY", value = "ANY"),
          function(x, i, j, ..., value) {
              # Mark new objects as not active for lazy mapping
              lazyTmMap <- meta(x, tag = "lazyTmMap", type = "corpus")
              if (!is.null(lazyTmMap)) {
                  lazyTmMap$index[i] <- FALSE
                  meta(x, tag = "lazyTmMap", type = "corpus") <- lazyTmMap
              }
              # Set the value
              x@.Data[[i, ...]] <- value

              x
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

    list(root = set_id(object), left.mapping = left.mapping, right.mapping = mapping)
}

setMethod("c",
          signature(x = "Corpus"),
          function(x, ..., meta = list(merge_date = as.POSIXlt(Sys.time(), tz = "GMT"), merger = Sys.getenv("LOGNAME")), recursive = FALSE) {
              args <- list(...)
              if (identical(length(args), 0)) return(x)

              if (!all(sapply(args, inherits, class(x))))
                  stop("not all arguments are of the same corpus type")

              if (inherits(x, "PCorpus"))
                  stop("concatenation of corpora with underlying databases is not supported")

              Reduce(c2, base::c(list(x), args))
          })

setGeneric("c2", function(x, y, ..., meta = list(merge_date = as.POSIXlt(Sys.time(), tz = "GMT"), merger = Sys.getenv("LOGNAME"))) standardGeneric("c2"))
#setMethod("c2", signature(x = "FCorpus", y = "FCorpus"),
#          function(x, y, ..., meta = list(merge_date = as.POSIXlt(Sys.time(), tz = "GMT"), merger = Sys.getenv("LOGNAME"))) {
#              new("FCorpus", .Data = c(as(x, "list"), as(y, "list")))
#          })
setMethod("c2", signature(x = "VCorpus", y = "VCorpus"),
          function(x, y, ..., meta = list(merge_date = as.POSIXlt(Sys.time(), tz = "GMT"), merger = Sys.getenv("LOGNAME"))) {
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

              object
          })

setMethod("c",
          signature(x = "TextDocument"),
          function(x, ..., recursive = FALSE){
              args <- list(...)
              if (identical(length(args), 0)) return(x)

              dmeta.df <- data.frame(MetaID = rep(0, length(list(x, ...))), stringsAsFactors = FALSE)
              cmeta.node <- new("MetaDataNode",
                            NodeID = 0,
                            MetaData = list(create_date = as.POSIXlt(Sys.time(), tz = "GMT"), creator = Sys.getenv("LOGNAME")),
                            children = list())

              new("VCorpus", .Data = list(x, ...), DMetaData = dmeta.df, CMetaData = cmeta.node)
          })

setMethod("show",
          signature(object = "Corpus"),
          function(object){
              cat(sprintf(ngettext(length(object),
                                   "A corpus with %d text document\n",
                                   "A corpus with %d text documents\n"),
                          length(object)))
    })

setMethod("summary",
          signature(object = "Corpus"),
          function(object){
              show(object)
              if (length(DMetaData(object)) > 0) {
                  cat(sprintf(ngettext(length(CMetaData(object)@MetaData),
                                              "\nThe metadata consists of %d tag-value pair and a data frame\n",
                                              "\nThe metadata consists of %d tag-value pairs and a data frame\n"),
                                       length(CMetaData(object)@MetaData)))
                  cat("Available tags are:\n")
                  cat(strwrap(paste(names(CMetaData(object)@MetaData), collapse = " "), indent = 2, exdent = 2), "\n")
                  cat("Available variables in the data frame are:\n")
                  cat(strwrap(paste(names(DMetaData(object)), collapse = " "), indent = 2, exdent = 2), "\n")
              }
    })

inspect <- function(x) UseMethod("inspect", x)
inspect.PCorpus <- function(x) {
    summary(x)
    cat("\n")
    db <- filehash::dbInit(DBControl(x)[["dbName"]], DBControl(x)[["dbType"]])
    show(filehash::dbMultiFetch(db, unlist(x)))
}
#inspect.FCorpus <-
inspect.VCorpus <- function(x) {
    summary(x)
    cat("\n")
    print(noquote(lapply(x, identity)))
}

# No metadata is checked
setGeneric("%IN%", function(x, y) standardGeneric("%IN%"))
setMethod("%IN%", signature(x = "TextDocument", y = "PCorpus"),
          function(x, y) {
              db <- filehash::dbInit(DBControl(y)[["dbName"]], DBControl(y)[["dbType"]])
              any(sapply(y, function(x, z) {x %in% Content(z)}, x))
          })
setMethod("%IN%", signature(x = "TextDocument", y = "VCorpus"),
          function(x, y) x %in% y)

setMethod("lapply",
          signature(X = "VCorpus"),
          function(X, FUN, ...) {
              lazyTmMap <- meta(X, tag = "lazyTmMap", type = "corpus")
              if (!is.null(lazyTmMap))
                  .Call("copyCorpus", X, materialize(X))
              base::lapply(X, FUN, ...)
          })
setMethod("lapply",
          signature(X = "PCorpus"),
          function(X, FUN, ...) {
              db <- filehash::dbInit(DBControl(X)[["dbName"]], DBControl(X)[["dbType"]])
              lapply(filehash::dbMultiFetch(db, unlist(X)), FUN, ...)
          })

setMethod("sapply",
          signature(X = "VCorpus"),
          function(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) {
              lazyTmMap <- meta(X, tag = "lazyTmMap", type = "corpus")
              if (!is.null(lazyTmMap))
                  .Call("copyCorpus", X, materialize(X))
              base::sapply(X, FUN, ...)
          })
setMethod("sapply",
          signature(X = "PCorpus"),
          function(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) {
              db <- filehash::dbInit(DBControl(X)[["dbName"]], DBControl(X)[["dbType"]])
              sapply(filehash::dbMultiFetch(db, unlist(X)), FUN, ...)
          })

setAs("list", "VCorpus", function(from) {
    cmeta.node <- new("MetaDataNode",
                      NodeID = 0,
                      MetaData = list(create_date = as.POSIXlt(Sys.time(), tz = "GMT"), creator = Sys.getenv("LOGNAME")),
                      children = list())
    data <- vector("list", length(from))
    counter <- 1
    for (f in from) {
        data[[counter]] <- new("PlainTextDocument",
                               .Data = f,
                               DateTimeStamp = as.POSIXlt(Sys.time(), tz = "GMT"),
                               ID = as.character(counter),
                               Language = "eng")
        counter <- counter + 1
    }
    new("VCorpus", .Data = data,
        DMetaData = data.frame(MetaID = rep(0, length(from)), stringsAsFactors = FALSE),
        CMetaData = cmeta.node)
})

setGeneric("writeCorpus", function(object, path = ".", filenames = NULL) standardGeneric("writeCorpus"))
setMethod("writeCorpus",
          signature(object = "Corpus"),
          function(object, path = ".", filenames = NULL) {
              filenames <- file.path(path,
                                     if (is.null(filenames)) sapply(object, function(x) sprintf("%s.txt", ID(x)))
                                     else filenames)
              i <- 1
              for (o in object) {
                  writeLines(asPlain(o), filenames[i])
                  i <- i + 1
              }
          })
