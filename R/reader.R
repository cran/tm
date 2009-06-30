## Author: Ingo Feinerer
## Reader

getReaders <- function()
    c("readDOC", "readGmane", "readHTML", "readNewsgroup", "readPDF", "readReut21578XML", "readReut21578XMLasPlain", "readPlain", "readRCV1", "readTabular")

readPlain <- FunctionGenerator(function(...) {
    function(elem, language, id) {
        doc <- new("PlainTextDocument")
        slot(doc, "ID", check = FALSE) <- id
        slot(doc, "DateTimeStamp", check = FALSE) <- as.POSIXlt(Sys.time(), tz = "GMT")
        slot(doc, "Language", check = FALSE) <- language
        slot(doc, ".Data", check = FALSE) <- elem$content
        doc
    }
})

#readSlim <- function(elem, language, id) {
#    doc <- as.character(elem$content)
#    class(doc) <- c("MinimalDocument", "character")
#    attr(doc, "ID") <- id
#    attr(doc, "Language") <- language
#    doc
#}

readXML <- FunctionGenerator(function(spec, doc, ...) {
    spec <- spec
    doc <- doc
    function(elem, language, id) {
        require("XML")

        tree <- XML::xmlInternalTreeParse(elem$content, asText = TRUE)
        for (n in setdiff(names(spec), ".Data"))
            meta(doc, n) <- .xml_content(tree, spec[[n]])

        slot(doc, ".Data", check = FALSE) <- if (".Data" %in% names(spec))
            .xml_content(tree, spec[[".Data"]])
        else
            structure(XML::xmlTreeParse(elem$content, asText = TRUE), class = "list") # Mask as list to bypass S4 checks

        XML::free(tree)

        slot(doc, "Language", check = FALSE) <- language

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

readReut21578XMLasPlain <- readXML(spec = list(Author = list("node", "/REUTERS/TEXT/AUTHOR"),
                                   .Data = list("node", "/REUTERS/TEXT/BODY"),
                                   DateTimeStamp = list("function", function(node)
                                   strptime(sapply(XML::getNodeSet(node, "/REUTERS/DATE"), XML::xmlValue),
                                            format = "%d-%B-%Y %H:%M:%S",
                                            tz = "GMT")),
                                   Description = list("unevaluated", ""),
                                   Heading = list("node", "/REUTERS/TEXT/TITLE"),
                                   ID = list("attribute", "/REUTERS/@NEWID"),
                                   Origin = list("unevaluated", "Reuters-21578 XML"),
                                   Topics = list("node", "/REUTERS/TOPICS/D")),
                                   doc = new("PlainTextDocument"))

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
    function(elem, language, id) {
        mail <- elem$content
        author <- gsub("From: ", "", grep("^From:", mail, value = TRUE))
        datetimestamp <- strptime(gsub("Date: ", "", grep("^Date:", mail, value = TRUE)),
                                  format = format,
                                  tz = "GMT")
        origin <- gsub("Path: ", "", grep("^Path:", mail, value = TRUE))
        heading <- gsub("Subject: ", "", grep("^Subject:", mail, value = TRUE))
        newsgroup <- gsub("Newsgroups: ", "", grep("^Newsgroups:", mail, value = TRUE))

        # The header is separated from the body by a blank line.
        # Reference: \url{http://en.wikipedia.org/wiki/E-mail#Internet_e-mail_format}
        for (index in seq_along(mail)) {
            if (mail[index] == "")
                break
        }
        content <- mail[(index + 1):length(mail)]

        doc <- new("NewsgroupDocument")
        slot(doc, ".Data", check = FALSE) <- content
        slot(doc, "Author", check = FALSE) <- author
        slot(doc, "DateTimeStamp", check = FALSE) <- datetimestamp
        slot(doc, "ID", check = FALSE) <- id
        slot(doc, "Origin", check = FALSE) <- origin
        slot(doc, "Heading", check = FALSE) <- heading
        slot(doc, "Language", check = FALSE) <- language
        slot(doc, "Newsgroup", check = FALSE) <- newsgroup
        doc
    }
})

# readDOC needs antiword installed to be able to extract the text
readDOC <- FunctionGenerator(function(AntiwordOptions = "", ...) {
    AntiwordOptions <- AntiwordOptions
    function(elem, language, id) {
        content <- system(paste("antiword", AntiwordOptions, shQuote(eval(elem$uri))), intern = TRUE)
        doc <- new("PlainTextDocument")
        slot(doc, ".Data", check = FALSE) <- content
        slot(doc, "DateTimeStamp", check = FALSE) <- as.POSIXlt(Sys.time(), tz = "GMT")
        slot(doc, "ID", check = FALSE) <- id
        slot(doc, "Language", check = FALSE) <- language
        doc
    }
})

# readPDF needs pdftotext and pdfinfo installed to be able to extract the text and meta information
readPDF <- FunctionGenerator(function(PdfinfoOptions = "", PdftotextOptions = "", ...) {
    PdfinfoOptions <- PdfinfoOptions
    PdftotextOptions <- PdftotextOptions
    function(elem, language, id) {
        meta <- system(paste("pdfinfo", PdfinfoOptions, shQuote(eval(elem$uri))), intern = TRUE)
        heading <- gsub("Title:[[:space:]]*", "", grep("Title:", meta, value = TRUE))
        author <- gsub("Author:[[:space:]]*", "", grep("Author:", meta, value = TRUE))
        datetimestamp <- strptime(gsub("CreationDate:[[:space:]]*", "",
                                       grep("CreationDate:", meta, value = TRUE)),
                                  format = "%a %b %d %H:%M:%S %Y",
                                  tz = "GMT")
        description <- gsub("Subject:[[:space:]]*", "", grep("Subject:", meta, value = TRUE))
        origin <- gsub("Creator:[[:space:]]*", "", grep("Creator:", meta, value = TRUE))

        content <- system(paste("pdftotext", PdftotextOptions, shQuote(eval(elem$uri)), "-"), intern = TRUE)
        doc <- new("PlainTextDocument")
        slot(doc, ".Data", check = FALSE) <- content
        slot(doc, "Author", check = FALSE) <- author
        slot(doc, "DateTimeStamp", check = FALSE) <- datetimestamp
        slot(doc, "Description", check = FALSE) <- description
        slot(doc, "ID", check = FALSE) <- id
        slot(doc, "Origin", check = FALSE) <- origin
        slot(doc, "Heading", check = FALSE) <- heading
        slot(doc, "Language", check = FALSE) <- language
        doc
    }
})

readHTML <- FunctionGenerator(function(...) {
    function(elem, language, id) {
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

        doc <- new("StructuredTextDocument")
        slot(doc, ".Data", check = FALSE) <- content
        slot(doc, "Author", check = FALSE) <- author
        slot(doc, "DateTimeStamp", check = FALSE) <- datetimestamp
        slot(doc, "Description", check = FALSE) <- description
        slot(doc, "ID", check = FALSE) <- id
        slot(doc, "Origin", check = FALSE) <- origin
        slot(doc, "Heading", check = FALSE) <- heading
        slot(doc, "Language", check = FALSE) <- language
        doc
    }
})

readTabular <- FunctionGenerator(function(mappings, ...) {
    mappings <- mappings
    function(elem, language, id) {
        doc <- new("PlainTextDocument")
        slot(doc, "DateTimeStamp", check = FALSE) <- as.POSIXlt(Sys.time(), tz = "GMT")
        slot(doc, "ID", check = FALSE) <- id
        slot(doc, "Language", check = FALSE) <- language

        for (n in setdiff(names(mappings), ".Data"))
            meta(doc, n) <- elem$content[, mappings[[n]]]

        if (".Data" %in% names(mappings))
            slot(doc, ".Data", check = FALSE) <- elem$content[, mappings[[".Data"]]]

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

    doc <- new("PlainTextDocument")
    slot(doc, ".Data", check = FALSE) <- content
    slot(doc, "Author", check = FALSE) <- Author(node)
    slot(doc, "DateTimeStamp", check = FALSE) <- DateTimeStamp(node)
    slot(doc, "Description", check = FALSE) <- Description(node)
    slot(doc, "ID", check = FALSE) <- ID(node)
    slot(doc, "Origin", check = FALSE) <- Origin(node)
    slot(doc, "Heading", check = FALSE) <- Heading(node)
    slot(doc, "Language", check = FALSE) <- Language(node)
    slot(doc, "LocalMetaData", check = FALSE) <- LocalMetaData(node)
    doc
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
    content <- .xml_value_if_not_null(node[["TEXT"]][["BODY"]], "")
    heading <- .xml_value_if_not_null(node[["TEXT"]][["TITLE"]], "")
    topics <- unlist(XML::xmlApply(node[["TOPICS"]], XML::xmlValue), use.names = FALSE)

    doc <- new("PlainTextDocument")
    slot(doc, ".Data", check = FALSE) <- content
    slot(doc, "Author", check = FALSE) <- author
    slot(doc, "DateTimeStamp", check = FALSE) <- datetimestamp
    slot(doc, "Description", check = FALSE) <- description
    slot(doc, "ID", check = FALSE) <- id
    slot(doc, "Origin", check = FALSE) <- "Reuters-21578 XML"
    slot(doc, "Heading", check = FALSE) <- heading
    slot(doc, "Language", check = FALSE) <- "eng"
    slot(doc, "LocalMetaData", check = FALSE) <- list(Topics = topics)
    doc
}
