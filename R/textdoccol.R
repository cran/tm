# Author: Ingo Feinerer

# The "..." are additional arguments for the FunctionGenerator reader
setGeneric("Corpus", function(object,
                                  readerControl = list(reader = object@DefaultReader, language = "en_US", load = TRUE),
                                  dbControl = list(useDb = FALSE, dbName = "", dbType = "DB1"),
                                  ...) standardGeneric("Corpus"))
setMethod("Corpus",
          signature(object = "Source"),
          function(object,
                   readerControl = list(reader = object@DefaultReader, language = "en_US", load = TRUE),
                   dbControl = list(useDb = FALSE, dbName = "", dbType = "DB1"),
                   ...) {
              if (is.null(readerControl$reader))
                  readerControl$reader <- object@DefaultReader
              if (is(readerControl$reader, "FunctionGenerator"))
                  readerControl$reader <- readerControl$reader(...)
              if (is.null(readerControl$language))
                  readerControl$language = "en_US"
              if (is.null(readerControl$load))
                  readerControl$load = TRUE

              if (dbControl$useDb) {
                  if (!dbCreate(dbControl$dbName, dbControl$dbType))
                      stop("error in creating database")
                  db <- dbInit(dbControl$dbName, dbControl$dbType)
              }

              # Allocate memory in advance if length is known
              tdl <- if (object@Length > 0)
                  vector("list", as.integer(object@Length))
              else
                  list()

              counter <- 1
              while (!eoi(object)) {
                  object <- stepNext(object)
                  elem <- getElem(object)
                  # If there is no Load on Demand support
                  # we need to load the corpus into memory at startup
                  if (!object@LoDSupport)
                      readerControl$load <- TRUE
                  doc <- readerControl$reader(elem, readerControl$load, readerControl$language, as.character(counter))
                  if (dbControl$useDb) {
                      dbInsert(db, ID(doc), doc)
                      if (object@Length > 0)
                          tdl[[counter]] <- ID(doc)
                      else
                          tdl <- c(tdl, ID(doc))
                  }
                  else {
                      if (object@Length > 0)
                          tdl[[counter]] <- doc
                      else
                          tdl <- c(tdl, list(doc))
                  }
                  counter <- counter + 1
              }

              df <- data.frame(MetaID = rep(0, length(tdl)), stringsAsFactors = FALSE)
              if (dbControl$useDb) {
                  dbInsert(db, "DMetaData", df)
                  dmeta.df <- data.frame(key = "DMetaData", subset = I(list(NA)))
              }
              else
                  dmeta.df <- df

              cmeta.node <- new("MetaDataNode",
                            NodeID = 0,
                            MetaData = list(create_date = Sys.time(), creator = Sys.getenv("LOGNAME")),
                            children = list())

              return(new("Corpus", .Data = tdl, DMetaData = dmeta.df, CMetaData = cmeta.node, DBControl = dbControl))
          })

setGeneric("loadDoc", function(object, ...) standardGeneric("loadDoc"))
setMethod("loadDoc",
          signature(object = "PlainTextDocument"),
          function(object, ...) {
              if (!Cached(object)) {
                  con <- eval(URI(object))
                  corpus <- readLines(con)
                  close(con)
                  Content(object) <- corpus
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
                  Content(object) <- doc
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
                  for (index in seq_along(mail)) {
                      if (mail[index] == "")
                          break
                  }
                  Content(object) <- mail[(index + 1):length(mail)]
                  return(object)
              } else {
                  return(object)
              }
          })
setMethod("loadDoc",
          signature(object = "StructuredTextDocument"),
          function(object, ...) {
              if (!Cached(object)) {
                  warning("load on demand not (yet) supported for StructuredTextDocuments")
                  return(object)
              } else
                  return(object)
          })

setGeneric("tmUpdate", function(object,
                                origin,
                                readerControl = list(reader = origin@DefaultReader, language = "en_US", load = TRUE),
                                ...) standardGeneric("tmUpdate"))
# Update is only supported for directories
# At the moment no other LoD devices are available anyway
setMethod("tmUpdate",
          signature(object = "Corpus", origin = "DirSource"),
          function(object, origin,
                   readerControl = list(reader = origin@DefaultReader, language = "en_US", load = TRUE),
                   ...) {
              if (is.null(readerControl$reader))
                  readerControl$reader <- origin@DefaultReader
              if (is(readerControl$reader, "FunctionGenerator"))
                  readerControl$reader <- readerControl$reader(...)
              if (is.null(readerControl$language))
                  readerControl$language = "en_US"
              if (is.null(readerControl$load))
                  readerControl$load = TRUE

              object.filelist <- unlist(lapply(object, function(x) {summary(eval(URI(x)))$description}))
              new.files <- setdiff(origin@FileList, object.filelist)

              for (filename in new.files) {
                  encoding <- origin@Encoding
                  elem <- list(content = readLines(filename, encoding = encoding),
                               uri = substitute(file(filename, encoding = encoding)))
                  object <- appendElem(object, readerControl$reader(elem, readerControl$load, readerControl$language, filename))
              }

              return(object)
          })

setGeneric("tmMap", function(object, FUN, ..., lazy = FALSE) standardGeneric("tmMap"))
setMethod("tmMap",
          signature(object = "Corpus", FUN = "function"),
          function(object, FUN, ..., lazy = FALSE) {
              result <- object
              # Note that text corpora are automatically loaded into memory via \code{[[}
              if (DBControl(object)[["useDb"]]) {
                  if (lazy)
                      warning("lazy mapping is deactived when using database backend")
                  db <- dbInit(DBControl(object)[["dbName"]], DBControl(object)[["dbType"]])
                  i <- 1
                  for (id in unlist(object)) {
                      db[[id]] <- FUN(object[[i]], ..., DMetaData = DMetaData(object))
                      i <- i + 1
                  }
                  # Suggested by Christian Buchta
                  dbReorganize(db)
              }
              else {
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
              }
              return(result)
          })

# Materialize lazy mappings
# Improvements by Christian Buchta
materialize <- function(corpus, range = seq_along(corpus)) {
    lazyTmMap <- meta(corpus, tag = "lazyTmMap", type = "corpus")
    if (!is.null(lazyTmMap)) {
       # Make valid and lazy index
       idx <- (seq_along(corpus) %in% range) & lazyTmMap$index
       if (any(idx)) {
           res <- lapply(corpus@.Data[idx], loadDoc)
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
    return(corpus)
}

setGeneric("asPlain", function(object, FUN, ...) standardGeneric("asPlain"))
setMethod("asPlain",
          signature(object = "PlainTextDocument"),
          function(object, FUN, ...) {
              return(object)
          })
setMethod("asPlain",
          signature(object = "XMLTextDocument"),
          function(object, FUN, ...) {
              corpus <- Content(object)

              # As XMLDocument is no native S4 class, restore valid information
              class(corpus) <- "XMLDocument"
              names(corpus) <- c("doc","dtd")

              return(FUN(xmlRoot(corpus), ...))
          })
setMethod("asPlain",
          signature(object = "Reuters21578Document"),
          function(object, FUN, ...) {
              FUN <- convertReut21578XMLPlain
              corpus <- Content(object)

              # As XMLDocument is no native S4 class, restore valid information
              class(corpus) <- "XMLDocument"
              names(corpus) <- c("doc","dtd")

              return(FUN(xmlRoot(corpus), ...))
          })
setMethod("asPlain",
          signature(object = "RCV1Document"),
          function(object, FUN, ...) {
              return(convertRCV1Plain(object, ...))
          })
setMethod("asPlain",
          signature(object = "NewsgroupDocument"),
          function(object, FUN, ...) {
              new("PlainTextDocument", .Data = Content(object), Cached = TRUE, URI = NULL, Author = Author(object),
                  DateTimeStamp = DateTimeStamp(object), Description = Description(object), ID = ID(object),
                  Origin = Origin(object), Heading = Heading(object), Language = Language(object),
                  LocalMetaData = LocalMetaData(object))
          })
setMethod("asPlain",
          signature(object = "StructuredTextDocument"),
          function(object, FUN, ...) {
              new("PlainTextDocument", .Data = unlist(Content(object)), Cached = TRUE,
                  URI = NULL, Author = Author(object), DateTimeStamp = DateTimeStamp(object),
                  Description = Description(object), ID = ID(object), Origin = Origin(object),
                  Heading = Heading(object), Language = Language(object),
                  LocalMetaData = LocalMetaData(object))
          })

setGeneric("tmFilter", function(object, ..., FUN = searchFullText, doclevel = TRUE) standardGeneric("tmFilter"))
setMethod("tmFilter",
          signature(object = "Corpus"),
          function(object, ..., FUN = searchFullText, doclevel = TRUE) {
              if (!is.null(attr(FUN, "doclevel")))
                  doclevel <- attr(FUN, "doclevel")
              if (doclevel) {
                  if (clusterAvailable())
                      return(object[snow::parSapply(snow::getMPIcluster(), object, FUN, ..., DMetaData = DMetaData(object))])
                  else
                      return(object[sapply(object, FUN, ..., DMetaData = DMetaData(object))])
              }
              else
                  return(object[FUN(object, ...)])
          })

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
              if (DBControl(object)[["useDb"]]) {
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

setGeneric("appendMeta", function(object, cmeta = NULL, dmeta = NULL) standardGeneric("appendMeta"))
setMethod("appendMeta",
          signature(object = "Corpus"),
          function(object, cmeta = NULL, dmeta = NULL) {
              object@CMetaData@MetaData <- c(CMetaData(object)@MetaData, cmeta)
              if (!is.null(dmeta)) {
                  DMetaData(object) <- cbind(DMetaData(object), eval(dmeta))
              }
              return(object)
          })

setGeneric("removeMeta", function(object, cname = NULL, dname = NULL) standardGeneric("removeMeta"))
setMethod("removeMeta",
          signature(object = "Corpus"),
          function(object, cname = NULL, dname = NULL) {
              if (!is.null(cname))
                  object@CMetaData@MetaData <- CMetaData(object)@MetaData[names(CMetaData(object)@MetaData) != cname]
              if (!is.null(dname))
                  DMetaData(object) <- DMetaData(object)[, names(DMetaData(object)) != dname, drop = FALSE]
              return(object)
          })

setGeneric("prescindMeta", function(object, meta) standardGeneric("prescindMeta"))
setMethod("prescindMeta",
          signature(object = "Corpus", meta = "character"),
          function(object, meta) {
              for (m in meta) {
                  if (m %in% c("Author", "DateTimeStamp", "Description", "ID", "Origin", "Heading", "Language")) {
                      local.m <- lapply(object, m)
                      local.m <- sapply(local.m, paste, collapse = " ")
                      local.m <- lapply(local.m, function(x) if (is.null(x)) return(NA) else return(x))
                      local.m <- unlist(local.m)
                      DMetaData(object) <- cbind(DMetaData(object), data.frame(m = local.m, stringsAsFactors = FALSE))
                      names(DMetaData(object))[which(names(DMetaData(object)) == "m")] <- m
                  }
                  else {
                      local.meta <- lapply(object, LocalMetaData)
                      local.m <- lapply(local.meta, "[[", m)
                      local.m <- lapply(local.m, function(x) if (is.null(x)) return(NA) else return(x))
                      if (length(local.m) == length(unlist(local.m)))
                          local.m <- unlist(local.m)
                      else
                          local.m <- I(local.m)
                      DMetaData(object) <- cbind(DMetaData(object), data.frame(m = local.m, stringsAsFactors = FALSE))
                      names(DMetaData(object))[which(names(DMetaData(object)) == "m")] <- m
                  }
              }
              return(object)
          })

setMethod("[",
          signature(x = "Corpus", i = "ANY", j = "ANY", drop = "ANY"),
          function(x, i, j, ... , drop) {
              if(missing(i))
                  return(x)

              object <- x
              object@.Data <- x@.Data[i, ..., drop = FALSE]
              if (DBControl(object)[["useDb"]]) {
                  index <- object@DMetaData[[1 , "subset"]]
                  if (any(is.na(index)))
                      object@DMetaData[[1 , "subset"]] <- i
                  else
                      object@DMetaData[[1 , "subset"]] <- index[i]
              }
              else
                  DMetaData(object) <- DMetaData(x)[i, , drop = FALSE]
              return(object)
          })

setMethod("[<-",
          signature(x = "Corpus", i = "ANY", j = "ANY", value = "ANY"),
          function(x, i, j, ... , value) {
              object <- x
              if (DBControl(object)[["useDb"]]) {
                  db <- dbInit(DBControl(object)[["dbName"]], DBControl(object)[["dbType"]])
                  counter <- 1
                  for (id in object@.Data[i, ...]) {
                      if (length(value) == 1)
                          db[[id]] <- value
                      else {
                          db[[id]] <- value[[counter]]
                      }
                      counter <- counter + 1
                  }
              }
              else
                  object@.Data[i, ...] <- value
              return(object)
          })

setMethod("[[",
          signature(x = "Corpus", i = "ANY", j = "ANY"),
          function(x, i, j, ...) {
              if (DBControl(x)[["useDb"]]) {
                  db <- dbInit(DBControl(x)[["dbName"]], DBControl(x)[["dbType"]])
                  result <- dbFetch(db, x@.Data[[i]])
                  return(loadDoc(result))
              }
              else {
                  lazyTmMap <- meta(x, tag = "lazyTmMap", type = "corpus")
                  if (!is.null(lazyTmMap))
                      .Call("copyCorpus", x, materialize(x, i))
                  return(loadDoc(x@.Data[[i]]))
              }
          })

setMethod("[[<-",
          signature(x = "Corpus", i = "ANY", j = "ANY", value = "ANY"),
          function(x, i, j, ..., value) {
              object <- x
              if (DBControl(object)[["useDb"]]) {
                  db <- dbInit(DBControl(object)[["dbName"]], DBControl(object)[["dbType"]])
                  index <- object@.Data[[i]]
                  db[[index]] <- value
              }
              else {
                  # Mark new objects as not active for lazy mapping
                  lazyTmMap <- meta(object, tag = "lazyTmMap", type = "corpus")
                  if (!is.null(lazyTmMap)) {
                      lazyTmMap$index[i] <- FALSE
                      meta(object, tag = "lazyTmMap", type = "corpus") <- lazyTmMap
                  }
                  # Set the value
                  object@.Data[[i, ...]] <- value
              }
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
          signature(x = "Corpus"),
          function(x, ..., meta = list(merge_date = Sys.time(), merger = Sys.getenv("LOGNAME")), recursive = TRUE) {
              args <- list(...)
              if (length(args) == 0)
                  return(x)

              if (!all(sapply(args, inherits, "Corpus")))
                  stop("not all arguments are text document collections")
              if (DBControl(x)[["useDb"]] == TRUE || any(unlist(sapply(args, DBControl)["useDb", ])))
                  stop("concatenating text document collections with activated database is not supported")

              result <- x
              for (c in args) {
                  result <- c2(result, c)
              }
              return(result)
          })

setGeneric("c2", function(x, y, ..., meta = list(merge_date = Sys.time(), merger = Sys.getenv("LOGNAME")), recursive = TRUE) standardGeneric("c2"))
setMethod("c2",
          signature(x = "Corpus", y = "Corpus"),
          function(x, y, ..., meta = list(merge_date = Sys.time(), merger = Sys.getenv("LOGNAME")), recursive = TRUE) {
              object <- x
              # Concatenate data slots
              object@.Data <- c(as(x, "list"), as(y, "list"))

              # Set the DBControl slot
              object@DBControl <- list(useDb = FALSE, dbName = "", dbType = "DB1")

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

              return(new("Corpus",
                         .Data = list(x, ...),
                         DMetaData = dmeta.df,
                         CMetaData = cmeta.node,
                         DBControl = list(useDb = FALSE, dbName = "", dbType = "DB1")))
          })

setMethod("length",
          signature(x = "Corpus"),
          function(x){
              return(length(as(x, "list")))
    })

setMethod("show",
          signature(object = "Corpus"),
          function(object){
              cat(sprintf(ngettext(length(object),
                                   "A text document collection with %d text document\n",
                                   "A text document collection with %d text documents\n"),
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

setGeneric("inspect", function(object) standardGeneric("inspect"))
setMethod("inspect",
          signature("Corpus"),
          function(object) {
              summary(object)
              cat("\n")
              if (DBControl(object)[["useDb"]]) {
                  db <- dbInit(DBControl(object)[["dbName"]], DBControl(object)[["dbType"]])
                  show(dbMultiFetch(db, unlist(object)))
              }
              else
                  print(noquote(lapply(object, identity)))
          })

# No metadata is checked
setGeneric("%IN%", function(x, y) standardGeneric("%IN%"))
setMethod("%IN%",
          signature(x = "TextDocument", y = "Corpus"),
          function(x, y) {
              if (DBControl(y)[["useDb"]]) {
                  db <- dbInit(DBControl(y)[["dbName"]], DBControl(y)[["dbType"]])
                  result <- any(sapply(y, function(x, z) {x %in% Content(z)}, x))
              }
              else
                  result <- x %in% y
              return(result)
          })

setMethod("lapply",
          signature(X = "Corpus"),
          function(X, FUN, ...) {
              if (DBControl(X)[["useDb"]]) {
                  db <- dbInit(DBControl(X)[["dbName"]], DBControl(X)[["dbType"]])
                  result <- lapply(dbMultiFetch(db, unlist(X)), FUN, ...)
              }
              else {
                  lazyTmMap <- meta(X, tag = "lazyTmMap", type = "corpus")
                  if (!is.null(lazyTmMap))
                      .Call("copyCorpus", X, materialize(X))
                  result <- base::lapply(X, FUN, ...)
              }
              return(result)
          })

setMethod("sapply",
          signature(X = "Corpus"),
          function(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) {
              if (DBControl(X)[["useDb"]]) {
                  db <- dbInit(DBControl(X)[["dbName"]], DBControl(X)[["dbType"]])
                  result <- sapply(dbMultiFetch(db, unlist(X)), FUN, ...)
              }
              else {
                  lazyTmMap <- meta(X, tag = "lazyTmMap", type = "corpus")
                  if (!is.null(lazyTmMap))
                      .Call("copyCorpus", X, materialize(X))
                  result <- base::sapply(X, FUN, ...)
              }
              return(result)
          })

setAs("list", "Corpus", function(from) {
    cmeta.node <- new("MetaDataNode",
                      NodeID = 0,
                      MetaData = list(create_date = Sys.time(), creator = Sys.getenv("LOGNAME")),
                      children = list())
    data <- list()
    counter <- 1
    for (f in from) {
        doc <- new("PlainTextDocument",
                   .Data = f, URI = NULL, Cached = TRUE,
                   Author = "", DateTimeStamp = Sys.time(),
                   Description = "", ID = as.character(counter),
                   Origin = "", Heading = "", Language = "en_US")
        data <- c(data, list(doc))
        counter <- counter + 1
    }
    return(new("Corpus", .Data = data,
               DMetaData = data.frame(MetaID = rep(0, length(from)), stringsAsFactors = FALSE),
               CMetaData = cmeta.node,
               DBControl = dbControl <- list(useDb = FALSE, dbName = "", dbType = "DB1")))
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
