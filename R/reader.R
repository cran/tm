# Author: Ingo Feinerer

# Reader

readPlain <- function(...) {
    function(elem, load, id) {
        doc <- if (load) {
            new("PlainTextDocument", .Data = elem$content, URI = elem$uri, Cached = TRUE,
                Author = "", DateTimeStamp = Sys.time(), Description = "", ID = id, Origin = "", Heading = "")
        }
        else {
            new("PlainTextDocument", URI = elem$uri, Cached = FALSE,
                Author = "", DateTimeStamp = Sys.time(), Description = "", ID = id, Origin = "", Heading = "")
        }

        return(doc)
    }
}
class(readPlain) <- "FunctionGenerator"

readReut21578XML <- function(...) {
    function(elem, load, id) {
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
            new("XMLTextDocument", .Data = tree, URI = elem$uri, Cached = TRUE, Author = author,
                DateTimeStamp = datetimestamp, Description = "", ID = id, Origin = "Reuters-21578 XML",
                Heading = heading, LocalMetaData = list(Topics = topics))
        } else {
            new("XMLTextDocument", URI = elem$uri, Cached = FALSE, Author = author,
                DateTimeStamp = datetimestamp, Description = "", ID = id, Origin = "Reuters-21578 XML",
                Heading = heading, LocalMetaData = list(Topics = topics))
        }

        return(doc)
    }
}
class(readReut21578XML) <- "FunctionGenerator"

readRCV1 <- function(...) {
    function(elem, load, id) {
        corpus <- paste(elem$content, "\n", collapse = "")
        tree <- xmlTreeParse(corpus, asText = TRUE)
        node <- xmlRoot(tree)

        # Mask as list to bypass S4 checks
        class(tree) <- "list"

        datetimestamp <- as.POSIXct(xmlAttrs(node)[["date"]])
        id <- xmlAttrs(node)[["itemid"]]
        heading <- xmlValue(node[["title"]])

        doc <- if (load) {
            new("XMLTextDocument", .Data = tree, URI = elem$uri, Cached = TRUE, Author = "",
                DateTimeStamp = datetimestamp, Description = "", ID = id, Origin = "Reuters Corpus Volume 1 XML",
                Heading = heading)
        } else {
            new("XMLTextDocument", URI = elem$uri, Cached = FALSE, Author = "",
                DateTimeStamp = datetimestamp, Description = "", ID = id, Origin = "Reuters Corpus Volume 1 XML",
                Heading = heading)
        }

        return(doc)
    }
}
class(readRCV1) <- "FunctionGenerator"

readNewsgroup <- function(...) {
    function(elem, load, id) {
        mail <- elem$content
        author <- gsub("From: ", "", grep("^From:", mail, value = TRUE))
        datetimestamp <- as.POSIXct(strptime(gsub("Date: ", "", grep("^Date:", mail, value = TRUE)), format = "%d %B %Y %H:%M:%S"))
        origin <- gsub("Path: ", "", grep("^Path:", mail, value = TRUE))
        heading <- gsub("Subject: ", "", grep("^Subject:", mail, value = TRUE))
        newsgroup <- gsub("Newsgroups: ", "", grep("^Newsgroups:", mail, value = TRUE))

        doc <- if (load) {
            # The header is separated from the body by a blank line.
            # Reference: \url{http://en.wikipedia.org/wiki/E-mail#Internet_e-mail_format}
            for (index in seq(along = mail)) {
                if (mail[index] == "")
                    break
            }
            content <- mail[(index + 1):length(mail)]

            new("NewsgroupDocument", .Data = content, URI = elem$uri, Cached = TRUE,
                Author = author, DateTimeStamp = datetimestamp,
                Description = "", ID = id, Origin = origin,
                Heading = heading, Newsgroup = newsgroup)
        } else {
            new("NewsgroupDocument", URI = elem$uri, Cached = FALSE, Author = author, DateTimeStamp = datetimestamp,
                Description = "", ID = id, Origin = origin, Heading = heading, Newsgroup = newsgroup)
        }

        return(doc)
    }
}
class(readNewsgroup) <- "FunctionGenerator"

readGmane <- function(...) {
    function(elem, load, id) {
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
                Heading = heading, Newsgroup = newsgroup)
        } else {
            new("NewsgroupDocument", URI = elem$uri, Cached = FALSE, Author = author, DateTimeStamp = datetimestamp,
                Description = "", ID = id, Origin = origin, Heading = heading, Newsgroup = newsgroup)
        }

        return(doc)
    }
}
class(readGmane) <- "FunctionGenerator"

# Converter

# Parse a <newsitem></newsitem> element from a well-formed RCV1 XML file
convertRCV1Plain <- function(node, ...) {
    datetimestamp <- as.POSIXct(xmlAttrs(node)[["date"]])
    id <- xmlAttrs(node)[["itemid"]]
    corpus <- unlist(xmlApply(node[["text"]], xmlValue), use.names = FALSE)
    heading <- xmlValue(node[["title"]])

    new("PlainTextDocument", .Data = corpus, Cached = TRUE, URI = "", Author = "", DateTimeStamp = datetimestamp,
        Description = "", ID = id, Origin = "Reuters Corpus Volume 1 XML", Heading = heading)
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

    new("PlainTextDocument", .Data = corpus, Cached = TRUE, URI = "", Author = author, DateTimeStamp = datetimestamp,
        Description = description, ID = id, Origin = "Reuters-21578 XML", Heading = heading, LocalMetaData = list(Topics = topics))
}
