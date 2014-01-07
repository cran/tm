## Author: Ingo Feinerer
## Sources

getSources <- function()
   c("DataframeSource", "DirSource", "ReutersSource", "URISource", "VectorSource")

Source <-
function(defaultReader = readPlain,
         encoding = "unknown",
         length = NA_integer_,
         names = NA_character_,
         position = 0,
         vectorized = TRUE,
         class)
{
    s <- structure(list(DefaultReader = defaultReader, Encoding = encoding,
                        Length = length, Names = names, Position = position,
                        Vectorized = vectorized),
                   class = unique(c(class, "Source")))
    stopifnot(is.Source(s))
    s
}

is.Source <-
function(x)
{
    iss <- FALSE
    if (!inherits(x, "Source"))
        warning("wrong class")
    else if (!is.list(x))
        warning("invalid structure")
    else if (!is.function(x$DefaultReader))
        warning("invalid default reader")
    else if (!is.character(x$Encoding))
        warning("invalid encoding")
    else if (!is.integer(x$Length))
        warning("invalid length entry denoting the number of elements")
    else if (!is.character(x$Names) && !is.null(x$Names))
        warning("invalid element names")
    else if (!is.numeric(x$Position))
        warning("invalid position")
    else if (!is.logical(x$Vectorized))
        warning("invalid indicator for parallel element access")
    else if (isTRUE(x$Vectorized) && (is.na(x$Length) || x$Length <= 0L))
        warning("vectorized sources must have a positive length entry")
    else if (!is.null(x$Names) && !is.na(x$Names) &&
             (x$Length != length(x$Names)))
        warning("incorrect number of element names")
    else
        iss <- TRUE
    iss
}

# A vector where each component is interpreted as document
VectorSource <- function(x, encoding = "unknown") {
    s <- Source(encoding = encoding, length = length(x), names = names(x),
                class = "VectorSource")
    s$Content <- if (is.factor(x)) as.character(x) else x 
    s
}

# A data frame where each row is interpreted as document
DataframeSource <- function(x, encoding = "unknown") {
    s <- Source(encoding = encoding, length = nrow(x), names = row.names(x),
                class = "DataframeSource")
    s$Content <- if (is.factor(x)) as.character(x) else x
    s
}

# A directory with files
DirSource <- function(directory = ".", encoding = "unknown", pattern = NULL, recursive = FALSE, ignore.case = FALSE) {
    d <- dir(directory, full.names = TRUE, pattern = pattern, recursive = recursive, ignore.case = ignore.case)

    if (!length(d))
        stop("empty directory")

    isfile <- !file.info(d)[["isdir"]]
    if (any(is.na(isfile)))
        stop("non-existent or non-readable file(s): ",
             paste(d[is.na(isfile)], collapse = " "))

    s <- Source(encoding = encoding, length = sum(isfile),
                names = basename(d[isfile]), class = "DirSource")
    s$FileList <- d[isfile]
    s
}

# Documents identified by a Uniform Resource Identifier
URISource <- function(x, encoding = "unknown") {
    s <- Source(encoding = encoding, length = length(x), class = "URISource")
    s$URI <- x
    s
}

ReutersSource <- function(x, encoding = "unknown")
    XMLSource(x, function(tree) XML::xmlChildren(XML::xmlRoot(tree)), readReut21578XML, encoding)

# XML
XMLSource <- function(x, parser, reader, encoding = "unknown") {
    tree <- XML::xmlParse(x, encoding = encoding)
    content <- parser(tree)
    XML::free(tree)

    s <- Source(defaultReader = reader, encoding = encoding,
                length = length(content), vectorized = FALSE,
                class = "XMLSource")
    s$Content <- content
    s$URI <- x
    s
}

stepNext <- function(x) UseMethod("stepNext", x)
stepNext.Source <- function(x) {
    x$Position <- x$Position + 1
    x
}

getElem <- function(x) UseMethod("getElem", x)
getElem.DataframeSource <- function(x) list(content = x$Content[x$Position, ], uri = NA)
getElem.DirSource <- function(x) {
    filename <- x$FileList[x$Position]
    encoding <- x$Encoding
    list(content = readLines(filename, encoding = encoding), uri = filename)
}
getElem.URISource <-
function(x) list(content = readLines(x$URI[x$Position], encoding = x$Encoding),
                 uri = x$URI[x$Position])
getElem.VectorSource <- function(x) list(content = x$Content[x$Position], uri = NA)
getElem.XMLSource <- function(x) list(content = XML::saveXML(x$Content[[x$Position]]), uri = x$URI)

pGetElem <- function(x) UseMethod("pGetElem", x)
pGetElem.DataframeSource <- function(x)
    lapply(seq_len(x$Length), function(y) list(content = x$Content[y,], uri = NA))
pGetElem.DirSource <- function(x)
    lapply(x$FileList, function(y) list(content = readLines(y, encoding = x$Encoding), uri = y))
pGetElem.URISource <- function(x)
    lapply(x$URI, function(y) list(content = readLines(y, encoding = x$Encoding), uri = y))
pGetElem.VectorSource <- function(x)
    lapply(x$Content, function(y) list(content = y, uri = NA))

eoi <- function(x) UseMethod("eoi", x)
eoi.Source <- function(x) x$Length <= x$Position
