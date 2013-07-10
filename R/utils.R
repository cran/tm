## Helper functions

.xml_value_if_not_null <- function(n, default) if (!is.null(n)) XML::xmlValue(n) else default

.xml_content <- function(doc, spec) {
    type <- spec[[1]]
    fun <- switch(type,
                  node = XML::xmlValue,
                  attribute = identity)

    if (identical(type, "unevaluated"))
        spec[[2]]
    else if (identical(type, "function") && is.function(spec[[2]]))
        spec[[2]](doc)
    else
        as.character(sapply(XML::getNodeSet(doc, spec[[2]]), fun))
}

# Map IETF language tags to languages used by the Snowball stemmer project
# http://en.wikipedia.org/wiki/IETF_language_tag
map_IETF_Snowball <- function(code) {
    if (identical(code, "") || identical(code, character(0)))
        return("porter")

    codes <- c("da", "nl", "en", "fi", "fr", "de", "hu", "it", "no", "pt", "ru", "es", "sv")
    names <- c("danish", "dutch", "english", "finnish", "french", "german", "hungarian",
               "italian", "norwegian", "portuguese", "russian", "spanish", "swedish")

    names[charmatch(gsub("-.*", "", code), codes)]
}

pdfinfo <- function(file) {
    outfile <- tempfile("pdfinfo")
    on.exit(unlink(outfile))
    status <- system2("pdfinfo", shQuote(normalizePath(file)),
                      stdout = outfile)
    ## Could check the status ...
    ## This does not work ...
    ##   info <- as.list(read.dcf(outfile)[1L, ])

    tags <- c("Title", "Subject", "Keywords", "Author", "Creator",
              "Producer", "CreationDate", "ModDate", "Tagged", "Form",
              "Pages", "Encrypted", "Page size", "File size",
              "Optimized", "PDF version" )
    re <- sprintf("^(%s)",
                  paste(sprintf("%-16s", sprintf("%s:", tags)),
                        collapse = "|"))
    lines <- readLines(outfile, warn = FALSE)
    ind <- grepl(re, lines)
    tags <- sub(": *", "", substring(lines[ind], 1L, 16L))
    info <- split(sub(re, "", lines), cumsum(ind))
    names(info) <- tags
    fmt <- "%a %b %d %X %Y"
    if(!is.null(d <- info$CreationDate))
        info$CreationDate <- strptime(d, fmt)
    if(!is.null(d <- info$ModDate))
        info$ModDate <- strptime(d, fmt)
    if(!is.null(p <- info$Pages))
        info$Pages <- as.integer(p)
    info
}
