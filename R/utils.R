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
    stopifnot(is.character(code))

    if (identical(code, "") || identical(code, character(0)))
        return("porter")

    codes <- c("da", "nl", "en", "fi", "fr", "de", "hu", "it", "no", "pt", "ru", "es", "sv")
    names <- c("danish", "dutch", "english", "finnish", "french", "german", "hungarian",
               "italian", "norwegian", "portuguese", "russian", "spanish", "swedish")

    names[charmatch(gsub("-.*", "", code), codes)]
}
