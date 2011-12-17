## Author: Ingo Feinerer
## Reader

getReaders <- function()
    c("readDOC", "readGmane", "readPDF", "readReut21578XML", "readReut21578XMLasPlain", "readPlain", "readRCV1", "readRCV1asPlain", "readTabular", "readXML")

prepareReader <- function(readerControl, defaultReader = NULL, ...) {
    if (is.null(readerControl$reader))
        readerControl$reader <- defaultReader
    if (inherits(readerControl$reader, "FunctionGenerator"))
        readerControl$reader <- readerControl$reader(...)
    if (is.null(readerControl$language))
        readerControl$language <- "en"
    readerControl
}

readPlain <- FunctionGenerator(function(...) {
    function(elem, language, id) PlainTextDocument(elem$content, id = id, language = language)
})

readXML <- FunctionGenerator(function(spec, doc, ...) {
    spec <- spec
    doc <- doc
    function(elem, language, id) {
        tree <- XML::xmlInternalTreeParse(elem$content, asText = TRUE)
        Content(doc) <- if ("Content" %in% names(spec))
            .xml_content(tree, spec[["Content"]])
        else
            XML::xmlTreeParse(elem$content, asText = TRUE)
        for (n in setdiff(names(spec), "Content"))
            meta(doc, n) <- .xml_content(tree, spec[[n]])
        XML::free(tree)
        attr(doc, "Language") <- language
        doc
    }
})

readGmane <- readXML(spec = list(Author = list("node", "/item/creator"),
                     Content = list("node", "/item/description"),
                     DateTimeStamp = list("function", function(node)
                     strptime(sapply(XML::getNodeSet(node, "/item/date"), XML::xmlValue),
                              format = "%Y-%m-%dT%H:%M:%S",
                              tz = "GMT")),
                     Description = list("unevaluated", ""),
                     Heading = list("node", "/item/title"),
                     ID = list("node", "/item/link"),
                     Origin = list("unevaluated", "Gmane Mailing List Archive")),
                     doc = PlainTextDocument())

readReut21578XML <- readXML(spec = list(Author = list("node", "/REUTERS/TEXT/AUTHOR"),
                            DateTimeStamp = list("function", function(node)
                            strptime(sapply(XML::getNodeSet(node, "/REUTERS/DATE"), XML::xmlValue),
                                     format = "%d-%B-%Y %H:%M:%S",
                                     tz = "GMT")),
                            Description = list("unevaluated", ""),
                            Heading = list("node", "/REUTERS/TEXT/TITLE"),
                            ID = list("attribute", "/REUTERS/@NEWID"),
                            TOPICS = list("attribute", "/REUTERS/@TOPICS"),
                            LEWISSPLIT = list("attribute", "/REUTERS/@LEWISSPLIT"),
                            CGISPLIT = list("attribute", "/REUTERS/@CGISPLIT"),
                            OLDID = list("attribute", "/REUTERS/@OLDID"),
                            Origin = list("unevaluated", "Reuters-21578 XML"),
                            Topics = list("node", "/REUTERS/TOPICS/D"),
                            Places = list("node", "/REUTERS/PLACES/D"),
                            People = list("node", "/REUTERS/PEOPLE/D"),
                            Orgs = list("node", "/REUTERS/ORGS/D"),
                            Exchanges = list("node", "/REUTERS/EXCHANGES/D")),
                            doc = Reuters21578Document())

readReut21578XMLasPlain <- readXML(spec = list(Author = list("node", "/REUTERS/TEXT/AUTHOR"),
                                   Content = list("node", "/REUTERS/TEXT/BODY"),
                                   DateTimeStamp = list("function", function(node)
                                   strptime(sapply(XML::getNodeSet(node, "/REUTERS/DATE"), XML::xmlValue),
                                            format = "%d-%B-%Y %H:%M:%S",
                                            tz = "GMT")),
                                   Description = list("unevaluated", ""),
                                   Heading = list("node", "/REUTERS/TEXT/TITLE"),
                                   ID = list("attribute", "/REUTERS/@NEWID"),
                                   TOPICS = list("attribute", "/REUTERS/@TOPICS"),
                                   LEWISSPLIT = list("attribute", "/REUTERS/@LEWISSPLIT"),
                                   CGISPLIT = list("attribute", "/REUTERS/@CGISPLIT"),
                                   OLDID = list("attribute", "/REUTERS/@OLDID"),
                                   Origin = list("unevaluated", "Reuters-21578 XML"),
                                   Topics = list("node", "/REUTERS/TOPICS/D"),
                                   Places = list("node", "/REUTERS/PLACES/D"),
                                   People = list("node", "/REUTERS/PEOPLE/D"),
                                   Orgs = list("node", "/REUTERS/ORGS/D"),
                                   Exchanges = list("node", "/REUTERS/EXCHANGES/D")),
                                   doc = PlainTextDocument())

readRCV1 <- readXML(spec = list(Author = list("unevaluated", ""),
                    DateTimeStamp = list("function", function(node)
                    as.POSIXlt(as.character(XML::getNodeSet(node, "/newsitem/@date")), tz = "GMT")),
                    Description = list("unevaluated", ""),
                    Heading = list("node", "/newsitem/title"),
                    ID = list("attribute", "/newsitem/@itemid"),
                    Origin = list("unevaluated", "Reuters Corpus Volume 1"),
                    Publisher = list("attribute", "/newsitem/metadata/dc[@element='dc.publisher']/@value"),
                    Topics = list("attribute", "/newsitem/metadata/codes[@class='bip:topics:1.0']/code/@code"),
                    Industries = list("attribute", "/newsitem/metadata/codes[@class='bip:industries:1.0']/code/@code"),
                    Countries = list("attribute", "/newsitem/metadata/codes[@class='bip:countries:1.0']/code/@code")),
                    doc = RCV1Document())

readRCV1asPlain <- readXML(spec = list(Author = list("unevaluated", ""),
                           Content = list("node", "/newsitem/text"),
                           DateTimeStamp = list("function", function(node)
                           as.POSIXlt(as.character(XML::getNodeSet(node, "/newsitem/@date")), tz = "GMT")),
                           Description = list("unevaluated", ""),
                           Heading = list("node", "/newsitem/title"),
                           ID = list("attribute", "/newsitem/@itemid"),
                           Origin = list("unevaluated", "Reuters Corpus Volume 1"),
                           Publisher = list("attribute", "/newsitem/metadata/dc[@element='dc.publisher']/@value"),
                           Topics = list("attribute", "/newsitem/metadata/codes[@class='bip:topics:1.0']/code/@code"),
                           Industries = list("attribute", "/newsitem/metadata/codes[@class='bip:industries:1.0']/code/@code"),
                           Countries = list("attribute", "/newsitem/metadata/codes[@class='bip:countries:1.0']/code/@code")),
                           doc = PlainTextDocument())

# readOOO needs unoconv (which in turn needs OpenOffice) installed
readOOO <- FunctionGenerator(function(unoconvOptions = "", ...) {
    unoconvOptions <- unoconvOptions
    function(elem, language, id) {
        tmp <- tempfile()
        # Unfortunately unoconv does not have an output file option and writes the output to the same directory as the input
        # In addition conversion to stdout may corrupt the zip file (odt) if writing it out via writeLines()
        if (!all(file.copy(elem$uri, sprintf("%s.oo", tmp))))
            stop(sprintf("cannot copy %s", elem$uri))
        system(paste("unoconv -f odt", sprintf("%s.oo", tmp)))
        meta.xml <- unzip(sprintf("%s.odt", tmp), "meta.xml", exdir = dirname(tmp))[1]

        on.exit(file.remove(sprintf("%s.oo", tmp), sprintf("%s.odt", tmp), meta.xml))

        root <- XML::xmlRoot(XML::xmlParse(meta.xml))

        content <- system(paste("unoconv -f txt --stdout", shQuote(elem$uri)), intern = TRUE)
        author <- XML::xpathSApply(root, "/office:document-meta/office:meta/dc:creator", XML::xmlValue)
        datetimestamp <- as.POSIXlt(XML::xpathSApply(root, "/office:document-meta/office:meta/dc:date", XML::xmlValue))

        PlainTextDocument(content, author, datetimestamp, id = id, language = language)
    }
})

# readDOC needs antiword installed to be able to extract the text
readDOC <- FunctionGenerator(function(AntiwordOptions = "", ...) {
    AntiwordOptions <- AntiwordOptions
    function(elem, language, id) {
        content <- system(paste("antiword", AntiwordOptions, shQuote(elem$uri)), intern = TRUE)
        PlainTextDocument(content, id = id, language = language)
    }
})

# readPDF needs pdftotext installed to be able to extract the text
readPDF <- FunctionGenerator(function(PdftotextOptions = "", ...) {
    PdftotextOptions <- PdftotextOptions
    function(elem, language, id) {
        meta <- tools:::pdf_info(elem$uri)
        content <- system(paste("pdftotext", PdftotextOptions, shQuote(elem$uri), "-"), intern = TRUE)
        PlainTextDocument(content, meta$Author, meta$CreationDate, meta$Subject, meta$Title, id, meta$Creator, language)
     }
})

readTabular <- FunctionGenerator(function(mapping, ...) {
    mapping <- mapping
    function(elem, language, id) {
        doc <- PlainTextDocument(id = id, language = language)
        for (n in names(mapping))
            content_meta(doc, n) <- elem$content[, mapping[[n]]]
        doc
    }
})
