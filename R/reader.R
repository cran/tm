## Author: Ingo Feinerer
## Reader

getReaders <- function()
    c("readDOC", "readGmane", "readHTML", "readNewsgroup", "readPDF", "readReut21578XML", "readPlain", "readRCV1", "readTabular")

readPlain <- FunctionGenerator(function(...) {
    function(elem, load, language, id) {
        doc <- if (load) {
            new("PlainTextDocument", .Data = elem$content, URI = elem$uri, Cached = TRUE,
                Author = "", DateTimeStamp = as.POSIXlt(Sys.time(), tz = "GMT"),
                Description = "", ID = id, Origin = "", Heading = "", Language = language)
        }
        else {
            new("PlainTextDocument", URI = elem$uri, Cached = FALSE,
                Author = "", DateTimeStamp = as.POSIXlt(Sys.time(), tz = "GMT"),
                Description = "", ID = id, Origin = "", Heading = "", Language = language)
        }

        return(doc)
    }
})

readXML <- FunctionGenerator(function(spec, doc, ...) {
    spec <- spec
    doc <- doc
    function(elem, load, language, id) {
        require("XML")

        tree <- XML::xmlInternalTreeParse(elem$content, asText = TRUE)
        for (n in setdiff(names(spec), ".Data"))
            meta(doc, n) <- .xml_content(tree, spec[[n]])
        if (load) {
            doc@.Data <- if (".Data" %in% names(spec))
                .xml_content(tree, spec[[".Data"]])
            else
                structure(XML::xmlTreeParse(elem$content, asText = TRUE), class = "list") # Mask as list to bypass S4 checks
        }
        XML::free(tree)

        doc@Cached <- load
        doc@URI <- elem$uri
        doc@Language <- language

        doc
    }
})

readGmane <- readXML(spec = list(Author = list("node", "/item/creator"),
                     .Data = list("node", "/item/description"),
                     DateTimeStamp = list("function", function(node)
                         strptime(sapply(XML::getNodeSet(node, "/item/date"), XML::xmlValue),
                                  format = "%Y-%m-%dT%H:%M:%S",
                                  tz = "GMT")),
                     Description = list("unevaluated", ""),
                     Heading = list("node", "/item/title"),
                     ID = list("node", "/item/link"),
                     Origin = list("unevaluated", "Gmane Mailing List Archive"),
                     Newsgroup = list("node", "/item/link")),
                     doc = new("NewsgroupDocument"))

readReut21578XML <- readXML(spec = list(Author = list("node", "/REUTERS/TEXT/AUTHOR"),
                            DateTimeStamp = list("function", function(node)
                                strptime(sapply(XML::getNodeSet(node, "/REUTERS/DATE"), XML::xmlValue),
                                         format = "%d-%B-%Y %H:%M:%S",
                                         tz = "GMT")),
                            Description = list("unevaluated", ""),
                            Heading = list("node", "/REUTERS/TEXT/TITLE"),
                            ID = list("attribute", "/REUTERS/@NEWID"),
                            Origin = list("unevaluated", "Reuters-21578 XML"),
                            Topics = list("node", "/REUTERS/TOPICS/D")),
                            doc = new("Reuters21578Document"))

readRCV1 <- readXML(spec = list(Author = list("unevaluated", ""),
                    DateTimeStamp = list("function", function(node)
                        as.POSIXlt(as.character(XML::getNodeSet(node, "/newsitem/@date")), tz = "GMT")),
                    Description = list("unevaluated", ""),
                    Heading = list("node", "/newsitem/title"),
                    ID = list("attribute", "/newsitem/@itemid"),
                    Origin = list("unevaluated", "Reuters Corpus Volume 1 XML"),
                    Publisher = list("attribute", "/newsitem/metadata/dc[@element='dc.publisher']/@value"),
                    Topics = list("attribute", "/newsitem/metadata/codes[@class='bip:topics:1.0']/code/@code")),
                    doc = new("RCV1Document"))

readNewsgroup <- FunctionGenerator(function(DateFormat = "%d %B %Y %H:%M:%S", ...) {
    format <- DateFormat
    function(elem, load, language, id) {
        mail <- elem$content
        author <- gsub("From: ", "", grep("^From:", mail, value = TRUE))
        datetimestamp <- strptime(gsub("Date: ", "", grep("^Date:", mail, value = TRUE)),
                                  format = format,
                                  tz = "GMT")
        origin <- gsub("Path: ", "", grep("^Path:", mail, value = TRUE))
        heading <- gsub("Subject: ", "", grep("^Subject:", mail, value = TRUE))
        newsgroup <- gsub("Newsgroups: ", "", grep("^Newsgroups:", mail, value = TRUE))

        doc <- if (load) {
            # The header is separated from the body by a blank line.
            # Reference: \url{http://en.wikipedia.org/wiki/E-mail#Internet_e-mail_format}
            for (index in seq_along(mail)) {
                if (mail[index] == "")
                    break
            }
            content <- mail[(index + 1):length(mail)]

            new("NewsgroupDocument", .Data = content, URI = elem$uri, Cached = TRUE,
                Author = author, DateTimeStamp = datetimestamp,
                Description = "", ID = id, Origin = origin,
                Heading = heading, Language = language, Newsgroup = newsgroup)
        } else {
            new("NewsgroupDocument", URI = elem$uri, Cached = FALSE, Author = author, DateTimeStamp = datetimestamp,
                Description = "", ID = id, Origin = origin, Heading = heading, Language = language, Newsgroup = newsgroup)
        }

        return(doc)
    }
})

# readDOC needs antiword installed to be able to extract the text
readDOC <- FunctionGenerator(function(AntiwordOptions = "", ...) {
    AntiwordOptions <- AntiwordOptions
    function(elem, load, language, id) {
        if (!load)
            warning("load on demand not supported for DOC documents")

        corpus <- system(paste("antiword", AntiwordOptions,
                               shQuote(summary(eval(elem$uri))$description)),
                         intern = TRUE)

        new("PlainTextDocument", .Data = corpus, URI = elem$uri, Cached = TRUE,
            Author = "", DateTimeStamp = as.POSIXlt(Sys.time(), tz = "GMT"), Description = "", ID = id,
            Origin = "", Heading = "", Language = language)
    }
})

# readPDF needs pdftotext and pdfinfo installed to be able to extract the text and meta information
readPDF <- FunctionGenerator(function(PdfinfoOptions = "", PdftotextOptions = "", ...) {
    PdfinfoOptions <- PdfinfoOptions
    PdftotextOptions <- PdftotextOptions
    function(elem, load, language, id) {
        meta <- system(paste("pdfinfo", PdfinfoOptions,
                             shQuote(summary(eval(elem$uri))$description)),
                       intern = TRUE)
        heading <- gsub("Title:[[:space:]]*", "", grep("Title:", meta, value = TRUE))
        author <- gsub("Author:[[:space:]]*", "", grep("Author:", meta, value = TRUE))
        datetimestamp <- strptime(gsub("CreationDate:[[:space:]]*", "",
                                       grep("CreationDate:", meta, value = TRUE)),
                                  format = "%a %b %d %H:%M:%S %Y",
                                  tz = "GMT")
        description <- gsub("Subject:[[:space:]]*", "", grep("Subject:", meta, value = TRUE))
        origin <- gsub("Creator:[[:space:]]*", "", grep("Creator:", meta, value = TRUE))

        if (!load)
            warning("load on demand not supported for PDF documents")

        corpus <- system(paste("pdftotext", PdftotextOptions,
                               shQuote(summary(eval(elem$uri))$description),
                               "-"),
                         intern = TRUE)
        new("PlainTextDocument", .Data = corpus, URI = elem$uri, Cached = TRUE,
            Author = author, DateTimeStamp = datetimestamp, Description = description, ID = id,
            Origin = origin, Heading = heading, Language = language)
    }
})

readHTML <- FunctionGenerator(function(...) {
    function(elem, load, language, id) {
        require("XML")

        tree <- XML::xmlTreeParse(elem$content, asText = TRUE)
        root <- XML::xmlRoot(tree)

        head <- root[["head"]]
        heading <- XML::xmlValue(head[["title"]])

        meta <- lapply(XML::xmlChildren(head)[names(XML::xmlChildren(head)) == "meta"], XML::xmlAttrs)
        metaNames <- sapply(meta, "[[", "name")
        metaContents <- sapply(meta, "[[", "content")

        # See http://dublincore.org/documents/dcmi-terms/ and http://dublincore.org/documents/dcq-html/
        author <- paste(metaContents[metaNames == "DC.creator"])
        description <- paste(metaContents[metaNames == "DC.description"])
        datetimestamp <- as.POSIXlt(paste(metaContents[metaNames == "DC.date"]), tz = "GMT")
        origin <- paste(metaContents[metaNames == "DC.publisher"])
        language <- paste(metaContents[metaNames == "DC.language"])

        if (!load)
            warning("load on demand not supported for StructuredTextDocuments using HTML")

        content <- list("Prologue" = NULL)
        i <- 1
        for (child in XML::xmlChildren(root[["body"]])) {
            if (tolower(XML::xmlName(child)) == "h1") {
                content <- c(content, structure(list(NULL), names = XML::xmlValue(child)))
                i <- i + 1
            }
            else {
                # We remove remaining HTML tags
                content[[i]] <- c(content[[i]], toString(XML::xmlApply(child, XML::xmlValue)))
            }
        }

        new("StructuredTextDocument", .Data = content, URI = elem$uri, Cached = TRUE,
            Author = author, DateTimeStamp = datetimestamp, Description = description, ID = id,
            Origin = origin, Heading = heading, Language = language)
    }
})

readTabular <- FunctionGenerator(function(mappings, ...) {
    mappings <- mappings
    function(elem, load, language, id) {
        doc <- new("PlainTextDocument", URI = elem$uri, Cached = load,
                   Author = "", DateTimeStamp = as.POSIXlt(Sys.time(), tz = "GMT"),
                   Description = "", ID = id, Origin = "", Heading = "", Language = language)

        for (n in setdiff(names(mappings), ".Data"))
            meta(doc, n) <- elem$content[, mappings[[n]]]

        if (load && (".Data" %in% names(mappings)))
            doc@.Data <- elem$content[, mappings[[".Data"]]]

        doc
    }
})

## Converter

convertRCV1Plain <- function(node, ...) {
    require("XML")

    content <- Content(node)
    # As XMLDocument is no native S4 class, restore valid information
    class(content) <- "XMLDocument"
    names(content) <- c("doc", "dtd")
    content <- unlist(XML::xmlApply(XML::xmlRoot(content)[["text"]], XML::xmlValue), use.names = FALSE)

    new("PlainTextDocument", .Data = content, Cached = TRUE, URI = NULL,
        Author = Author(node), DateTimeStamp = DateTimeStamp(node),
        Description = Description(node), ID = ID(node), Origin =
        Origin(node), Heading = Heading(node), Language = Language(node),
        LocalMetaData = LocalMetaData(node))
}

# Parse a <REUTERS></REUTERS> element from a well-formed Reuters-21578 XML file
convertReut21578XMLPlain <- function(node, ...) {
    require("XML")

    author <- .xml_value_if_not_null(node[["TEXT"]][["AUTHOR"]], "")
    datetimestamp <- strptime(XML::xmlValue(node[["DATE"]]),
                              format = "%d-%B-%Y %H:%M:%S",
                              tz = "GMT")
    description <- ""
    id <- XML::xmlAttrs(node)[["NEWID"]]
    corpus <- .xml_value_if_not_null(node[["TEXT"]][["BODY"]], "")
    heading <- .xml_value_if_not_null(node[["TEXT"]][["TITLE"]], "")
    topics <- unlist(XML::xmlApply(node[["TOPICS"]], XML::xmlValue), use.names = FALSE)

    new("PlainTextDocument", .Data = corpus, Cached = TRUE, URI = NULL, Author = author, DateTimeStamp = datetimestamp,
        Description = description, ID = id, Origin = "Reuters-21578 XML", Heading = heading, Language = "en_US",
        LocalMetaData = list(Topics = topics))
}
