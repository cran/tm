# Author: Ingo Feinerer
# Reader

getReaders <- function()
    c("readDOC", "readGmane", "readHTML", "readNewsgroup", "readPDF", "readReut21578XML", "readPlain", "readRCV1")

readPlain <- FunctionGenerator(function(...) {
    function(elem, load, language, id) {
        doc <- if (load) {
            new("PlainTextDocument", .Data = elem$content, URI = elem$uri, Cached = TRUE,
                Author = "", DateTimeStamp = Sys.time(), Description = "", ID = id, Origin = "", Heading = "", Language = language)
        }
        else {
            new("PlainTextDocument", URI = elem$uri, Cached = FALSE,
                Author = "", DateTimeStamp = Sys.time(), Description = "", ID = id, Origin = "", Heading = "", Language = language)
        }

        return(doc)
    }
})

readReut21578XML <- FunctionGenerator(function(...) {
    function(elem, load, language, id) {
        corpus <- paste(elem$content, "\n", collapse = "")
        tree <- xmlTreeParse(corpus, asText = TRUE)
        node <- xmlRoot(tree)

        # Mask as list to bypass S4 checks
        class(tree) <- "list"

        # The <AUTHOR></AUTHOR> tag is unfortunately NOT obligatory!
        author <- if (!is.null(node[["TEXT"]][["AUTHOR"]]))
            xmlValue(node[["TEXT"]][["AUTHOR"]])
        else
            ""

        datetimestamp <- as.POSIXct(strptime(xmlValue(node[["DATE"]]), format = "%d-%B-%Y %H:%M:%S"))
        id <- xmlAttrs(node)[["NEWID"]]

        # The <TITLE></TITLE> tag is unfortunately NOT obligatory!
        heading <- if (!is.null(node[["TEXT"]][["TITLE"]]))
            xmlValue(node[["TEXT"]][["TITLE"]])
        else
            ""

        topics <- unlist(xmlApply(node[["TOPICS"]], function(x) xmlValue(x)), use.names = FALSE)

        doc <- if (load) {
            new("Reuters21578Document", .Data = tree, URI = elem$uri, Cached = TRUE, Author = author,
                DateTimeStamp = datetimestamp, Description = "", ID = id, Origin = "Reuters-21578 XML",
                Heading = heading, Language = language, LocalMetaData = list(Topics = topics))
        } else {
            new("Reuters21578Document", URI = elem$uri, Cached = FALSE, Author = author,
                DateTimeStamp = datetimestamp, Description = "", ID = id, Origin = "Reuters-21578 XML",
                Heading = heading, Language = language, LocalMetaData = list(Topics = topics))
        }

        return(doc)
    }
})

readRCV1 <- FunctionGenerator(function(...) {
    function(elem, load, language, id) {
        corpus <- paste(elem$content, "\n", collapse = "")
        tree <- xmlTreeParse(corpus, asText = TRUE)
        node <- xmlRoot(tree)

        # Mask as list to bypass S4 checks
        class(tree) <- "list"

        datetimestamp <- as.POSIXct(xmlAttrs(node)[["date"]])
        id <- xmlAttrs(node)[["itemid"]]
        heading <- xmlValue(node[["title"]])

        doc <- if (load) {
            new("RCV1Document", .Data = tree, URI = elem$uri, Cached = TRUE, Author = "",
                DateTimeStamp = datetimestamp, Description = "", ID = id, Origin = "Reuters Corpus Volume 1 XML",
                Heading = heading, Language = language)
        } else {
            new("RCV1Document", URI = elem$uri, Cached = FALSE, Author = "",
                DateTimeStamp = datetimestamp, Description = "", ID = id, Origin = "Reuters Corpus Volume 1 XML",
                Heading = heading, Language = language)
        }

        # Extract meta data
        if (!is.null(node[["metadata"]])) {
            # Get simple Dublin Core meta data
            dc <- node[["metadata"]][names(node[["metadata"]]) == "dc"]
            dc <- lapply(dc, xmlAttrs)
            elements <- sapply(dc, "[[", "element")
            values <- sapply(dc, "[[", "value")
            if ("dc.publisher" %in% elements)
                DublinCore(doc, "Publisher") <- values[elements == "dc.publisher"]

            # Get topic codes
            codes <- node[["metadata"]][names(node[["metadata"]]) == "codes"]
            topics <- codes[sapply(codes, xmlAttrs) == "bip:topics:1.0"]
            if (length(topics) > 0)
                meta(doc, "Topics") <- unlist(xmlApply(topics[[1]], xmlAttrs), use.names = FALSE)
        }

        return(doc)
    }
})

readNewsgroup <- FunctionGenerator(function(...) {
    function(elem, load, language, id) {
        mail <- elem$content
        author <- gsub("From: ", "", grep("^From:", mail, value = TRUE))
        datetimestamp <- as.POSIXct(strptime(gsub("Date: ", "", grep("^Date:", mail, value = TRUE)), format = "%d %B %Y %H:%M:%S"))
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

readGmane <- FunctionGenerator(function(...) {
    function(elem, load, language, id) {
        corpus <- paste(elem$content, "\n", collapse = "")
        # Remove namespaces
        corpus <- gsub("dc:date", "date", corpus)
        corpus <- gsub("dc:creator", "creator", corpus)
        tree <- xmlTreeParse(corpus, asText = TRUE)
        node <- xmlRoot(tree)

        author <- xmlValue(node[["creator"]])
        datetimestamp <- as.POSIXct(strptime(xmlValue(node[["date"]]), format = "%Y-%m-%dT%H:%M:%S"))
        heading <- xmlValue(node[["title"]])
        id <- xmlValue(node[["link"]])
        newsgroup <- gsub("[0-9]+", "", xmlValue(node[["link"]]))
        origin <- "Gmane Mailing List Archive"

        doc <- if (load) {
            content <- xmlValue(node[["description"]])

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
readDOC <- FunctionGenerator(function(...) {
    function(elem, load, language, id) {
        if (!load)
            warning("load on demand not supported for DOC documents")

        corpus <- paste(system(paste("antiword", shQuote(summary(eval(elem$uri))$description)), intern = TRUE), sep = "\n", collapse = "")

        new("PlainTextDocument", .Data = corpus, URI = elem$uri, Cached = TRUE,
            Author = "", DateTimeStamp = Sys.time(), Description = "", ID = id,
            Origin = "", Heading = "", Language = language)
    }
})

# readPDF needs pdftotext and pdfinfo installed to be able to extract the text and meta information
readPDF <- FunctionGenerator(function(...) {
    function(elem, load, language, id) {
        meta <- system(paste("pdfinfo", shQuote(summary(eval(elem$uri))$description)), intern = TRUE)
        heading <- gsub("Title:[[:space:]]*", "", grep("Title:", meta, value = TRUE))
        author <- gsub("Author:[[:space:]]*", "", grep("Author:", meta, value = TRUE))
        datetimestamp <- as.POSIXct(strptime(gsub("CreationDate:[[:space:]]*", "",
                                                  grep("CreationDate:", meta, value = TRUE)),
                                             format = "%a %b %d %H:%M:%S %Y"))
        description <- gsub("Subject:[[:space:]]*", "", grep("Subject:", meta, value = TRUE))
        origin <- gsub("Creator:[[:space:]]*", "", grep("Creator:", meta, value = TRUE))

        if (!load)
            warning("load on demand not supported for PDF documents")

        corpus <- paste(system(paste("pdftotext", shQuote(summary(eval(elem$uri))$description), "-"), intern = TRUE), sep = "\n", collapse = "")
        new("PlainTextDocument", .Data = corpus, URI = elem$uri, Cached = TRUE,
            Author = author, DateTimeStamp = datetimestamp, Description = description, ID = id,
            Origin = origin, Heading = heading, Language = language)
    }
})

readHTML <- FunctionGenerator(function(...) {
    function(elem, load, language, id) {
        tree <- xmlTreeParse(elem$content, asText = TRUE)
        root <- xmlRoot(tree)

        head <- root[["head"]]
        heading <- xmlValue(head[["title"]])

        meta <- lapply(xmlChildren(head)[names(xmlChildren(head)) == "meta"], xmlAttrs)
        metaNames <- sapply(meta, "[[", "name")
        metaContents <- sapply(meta, "[[", "content")

        # See http://dublincore.org/documents/dcmi-terms/ and http://dublincore.org/documents/dcq-html/
        author <- paste(metaContents[metaNames == "DC.creator"])
        description <- paste(metaContents[metaNames == "DC.description"])
        datetimestamp <- as.POSIXct(paste(metaContents[metaNames == "DC.date"]))
        origin <- paste(metaContents[metaNames == "DC.publisher"])
        language <- paste(metaContents[metaNames == "DC.language"])

        if (!load)
            warning("load on demand not supported for StructuredTextDocuments using HTML")

        content <- list("Prologue" = NULL)
        i <- 1
        for (child in xmlChildren(root[["body"]])) {
            if (tolower(xmlName(child)) == "h1") {
                content <- c(content, structure(list(NULL), names = xmlValue(child)))
                i <- i + 1
            }
            else {
                # We remove remaining HTML tags
                content[[i]] <- c(content[[i]], toString(xmlApply(child, xmlValue)))
            }
        }

        new("StructuredTextDocument", .Data = content, URI = elem$uri, Cached = TRUE,
            Author = author, DateTimeStamp = datetimestamp, Description = description, ID = id,
            Origin = origin, Heading = heading, Language = language)
    }
})

# Converter

convertRCV1Plain <- function(node, ...) {
    content <- Content(node)
    # As XMLDocument is no native S4 class, restore valid information
    class(content) <- "XMLDocument"
    names(content) <- c("doc", "dtd")
    content <- unlist(xmlApply(xmlRoot(content)[["text"]], xmlValue), use.names = FALSE)

    new("PlainTextDocument", .Data = content, Cached = TRUE, URI = NULL,
        Author = Author(node), DateTimeStamp = DateTimeStamp(node),
        Description = Description(node), ID = ID(node), Origin =
        Origin(node), Heading = Heading(node), Language = Language(node),
        LocalMetaData = LocalMetaData(node))
}

# Parse a <REUTERS></REUTERS> element from a well-formed Reuters-21578 XML file
convertReut21578XMLPlain <- function(node, ...) {
    # The <AUTHOR></AUTHOR> tag is unfortunately NOT obligatory!
    if (!is.null(node[["TEXT"]][["AUTHOR"]]))
        author <- xmlValue(node[["TEXT"]][["AUTHOR"]])
    else
        author <- ""

    datetimestamp <- as.POSIXct(strptime(xmlValue(node[["DATE"]]), format = "%d-%B-%Y %H:%M:%S"))
    description <- ""
    id <- xmlAttrs(node)[["NEWID"]]

    # The <BODY></BODY> tag is unfortunately NOT obligatory!
    corpus <- if (!is.null(node[["TEXT"]][["BODY"]]))
        xmlValue(node[["TEXT"]][["BODY"]])
    else
        ""

    # The <TITLE></TITLE> tag is unfortunately NOT obligatory!
    heading <- if (!is.null(node[["TEXT"]][["TITLE"]]))
        xmlValue(node[["TEXT"]][["TITLE"]])
    else
        ""

    topics <- unlist(xmlApply(node[["TOPICS"]], function(x) xmlValue(x)), use.names = FALSE)

    new("PlainTextDocument", .Data = corpus, Cached = TRUE, URI = NULL, Author = author, DateTimeStamp = datetimestamp,
        Description = description, ID = id, Origin = "Reuters-21578 XML", Heading = heading, Language = "en_US",
        LocalMetaData = list(Topics = topics))
}
