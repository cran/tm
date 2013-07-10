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
        tree <- XML::xmlParse(elem$content, asText = TRUE)
        Content(doc) <- if ("Content" %in% names(spec))
            .xml_content(tree, spec[["Content"]])
        else
            XML::xmlTreeParse(elem$content, asText = TRUE)
        for (n in setdiff(names(spec), "Content"))
            meta(doc, n) <- .xml_content(tree, spec[[n]])
        XML::free(tree)
        if (!is.na(language))
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

# readDOC needs antiword installed to be able to extract the text
readDOC <- FunctionGenerator(function(AntiwordOptions = "", ...) {
    AntiwordOptions <- AntiwordOptions
    function(elem, language, id) {
        content <- system2("antiword",
                           c(AntiwordOptions, shQuote(elem$uri)),
                           stdout = TRUE)
        PlainTextDocument(content, id = id, language = language)
    }
})

# readPDF needs pdfinfo and pdftotext installed to extract meta data and text
readPDF <- FunctionGenerator(function(PdftotextOptions = "", ...) {
    PdftotextOptions <- PdftotextOptions
    function(elem, language, id) {
        meta <- tm:::pdfinfo(elem$uri)
        content <- system2("pdftotext",
                           c(PdftotextOptions, shQuote(elem$uri), "-"),
                           stdout = TRUE)
        PlainTextDocument(content, meta$Author, meta$CreationDate, meta$Subject,
                          meta$Title, id, meta$Creator, language)
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
