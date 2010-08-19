# Map IETF language tags to languages used by the Snowball stemmer
# http://en.wikipedia.org/wiki/IETF_language_tag
map_IETF <- function(code) {
    codes <- c("da", "nl", "en", "fi", "fr", "de", "hu", "it", "no", "pt", "ru", "es", "sv")
    names <- c("danish", "dutch", "english", "finnish", "french", "german", "hungarian",
               "italian", "norwegian", "portuguese", "russian", "spanish", "swedish")

    names[charmatch(gsub("-.*", "", code), codes)]
}

stopwords <- {
    function(language = "en") {
        resolved <- tm:::map_IETF(language)
        lang <- if (is.na(resolved))
            language
        else
            resolved
        readLines(system.file("stopwords", paste(lang, ".dat", sep = ""), package = "tm"), encoding = "UTF-8")
    }
}
