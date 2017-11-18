## Author: Ingo Feinerer
## Readers

FunctionGenerator <-
function(x)
{
    class(x) <- c("FunctionGenerator", "function")
    x
}

getReaders <-
function()
    c("readDataframe", "readDOC", "readPDF", "readPlain", "readRCV1",
      "readRCV1asPlain", "readReut21578XML", "readReut21578XMLasPlain",
      "readTagged", "readXML")

prepareReader <-
function(readerControl, reader = NULL, ...)
{
    if (is.null(readerControl$reader))
        readerControl$reader <- reader
    if (inherits(readerControl$reader, "FunctionGenerator"))
        readerControl$reader <- readerControl$reader(...)
    if (is.null(readerControl$language))
        readerControl$language <- "en"
    readerControl
}

processURI <-
function(uri)
{
    uri <- as.character(uri)
    if (identical(substr(uri, 1, 7), "file://"))
        uri <- substr(uri, 8, nchar(uri))
    uri
}

readDataframe <-
function(elem, language, id) {
    PlainTextDocument(elem$content[, "text"],
                      id = elem$content[, "doc_id"],
                      language = language)
}

# readDOC needs antiword installed to be able to extract the text
readDOC <-
function(engine = c("antiword", "executable"),
         AntiwordOptions = "")
{
    stopifnot(is.character(engine), is.character(AntiwordOptions))

    engine <- match.arg(engine)

    antiword <-
        switch(engine,
               antiword = antiword::antiword,
               executable = function(x)
                   system2("antiword",
                           c(AntiwordOptions, shQuote(normalizePath(x))),
                           stdout = TRUE))

    if (!is.function(antiword))
        stop("invalid function for DOC extraction")

    function(elem, language, id) {
        uri <- processURI(elem$uri)
        content <- antiword(uri)
        PlainTextDocument(content, id = basename(elem$uri), language = language)
    }
}
class(readDOC) <- c("FunctionGenerator", "function")

readPDF <-
function(engine = c("pdftools", "xpdf", "Rpoppler",
                    "ghostscript", "Rcampdf", "custom"),
         control = list(info = NULL, text = NULL))
{
    stopifnot(is.character(engine), is.list(control))

    engine <- match.arg(engine)

    pdf_info <-
        switch(engine,
               pdftools = function(x) {
                   i <- pdftools::pdf_info(x)
                   c(i$keys, list(CreationDate = i$created))
               },
               xpdf = function(x) pdf_info_via_xpdf(x, control$info),
               Rpoppler = Rpoppler::PDF_info,
               ghostscript = pdf_info_via_gs,
               Rcampdf = Rcampdf::pdf_info,
               custom = control$info)

    pdf_text <-
        switch(engine,
               pdftools = pdftools::pdf_text,
               xpdf = function(x) system2("pdftotext",
                                          c(control$text, shQuote(x), "-"),
                                          stdout = TRUE),
               Rpoppler = Rpoppler::PDF_text,
               ghostscript = pdf_text_via_gs,
               Rcampdf = Rcampdf::pdf_text,
               custom = control$text)

    if (!is.function(pdf_info) || !is.function(pdf_text))
        stop("invalid function for PDF extraction")

    function(elem, language, id) {
        uri <- processURI(elem$uri)
        meta <- pdf_info(uri)
        content <- pdf_text(uri)
        PlainTextDocument(content, meta$Author, meta$CreationDate, meta$Subject,
                          meta$Title, basename(elem$uri), language,
                          meta$Creator)
     }
}
class(readPDF) <- c("FunctionGenerator", "function")

readPlain <-
function(elem, language, id) {
    if (!is.null(elem$uri))
        id <- basename(elem$uri)
    PlainTextDocument(elem$content, id = id, language = language)
}

readXML <-
function(spec, doc)
{
    stopifnot(is.list(spec), inherits(doc, "TextDocument"))

    function(elem, language, id) {
        content <- elem$content
        node <- if(inherits(content, "xml_node"))
            content
        else if(is.character(content)) 
            read_xml(paste(elem$content, collapse = "\n"))
        else
            read_xml(content)
        content(doc) <- if ("content" %in% names(spec))
            .xml_content(node, spec[["content"]])
        else
            node
        for (n in setdiff(names(spec), "content"))
            meta(doc, n) <- .xml_content(node, spec[[n]])
        if (!is.null(elem$uri))
            id <- basename(elem$uri)
        if (!length(meta(doc, "id")))
            meta(doc, "id") <- as.character(id)
        if (!length(meta(doc, "language")))
            meta(doc, "language") <- as.character(language)
        doc
    }
}
class(readXML) <- c("FunctionGenerator", "function")

RCV1Spec <-
    list(author = list("unevaluated", ""),
         datetimestamp = list("function", function(node)
           as.POSIXlt(xml_text(xml_find_all(node, "@date")),
                      tz = "GMT")),
         description = list("unevaluated", ""),
         heading = list("node", "title"),
         id = list("node", "@itemid"),
         origin = list("unevaluated", "Reuters Corpus Volume 1"),
         publisher = list("node",
           "metadata/dc[@element='dc.publisher']/@value"),
         topics = list("node",
           "metadata/codes[@class='bip:topics:1.0']/code/@code"),
         industries = list("node",
           "metadata/codes[@class='bip:industries:1.0']/code/@code"),
         countries = list("node",
           "metadata/codes[@class='bip:countries:1.0']/code/@code"))
readRCV1 <-
    readXML(spec = RCV1Spec,
            doc = XMLTextDocument())
readRCV1asPlain <-
    readXML(spec = c(RCV1Spec,
                     list(content = list("node", "text"))),
            doc = PlainTextDocument())

Reut21578XMLSpec <-
    list(author = list("node", "TEXT/AUTHOR"),
         datetimestamp = list("function", function(node)
           strptime(xml_text(xml_find_all(node, "DATE")),
                    format = "%d-%B-%Y %H:%M:%S",
                    tz = "GMT")),
         description = list("unevaluated", ""),
         heading = list("node", "TEXT/TITLE"),
         id = list("node", "@NEWID"),
         topics = list("node", "@TOPICS"),
         lewissplit = list("node", "@LEWISSPLIT"),
         cgisplit = list("node", "@CGISPLIT"),
         oldid = list("node", "@OLDID"),
         origin = list("unevaluated", "Reuters-21578 XML"),
         topics_cat = list("node", "TOPICS/D"),
         places = list("node", "PLACES/D"),
         people = list("node", "PEOPLE/D"),
         orgs = list("node", "ORGS/D"),
         exchanges = list("node", "EXCHANGES/D"))
readReut21578XML <-
    readXML(spec = Reut21578XMLSpec,
            doc = XMLTextDocument())
readReut21578XMLasPlain <-
    readXML(spec = c(Reut21578XMLSpec,
                     list(content = list("node", "TEXT/BODY"))),
            doc = PlainTextDocument())

readTagged <-
function(...)
{
    args <- list(...)
    function(elem, language, id) {
        if (!is.null(elem$content)) {
            con <- textConnection(elem$content)
            on.exit(close(con))
        } else
            con <- elem$uri

        if (!is.null(elem$uri))
            id <- basename(elem$uri)

        a <- c(list(con = con, meta = list(id = id, language = language)), args)

        do.call(TaggedTextDocument, a)
    }
}
class(readTagged) <- c("FunctionGenerator", "function")
