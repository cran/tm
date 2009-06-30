# Map ISO 639-2 codes to languages used by the Snowball stemmer
# http://en.wikipedia.org/wiki/ISO_639-2
map_ISO_639_2 <- function(code) {
    codes <- c("dan", "nld", "dut", "eng", "fin", "fra", "fre", "deu",
               "ger", "hun", "ita", "nor", "por", "rus", "spa", "swe")
    names <- c("danish", "dutch", "dutch", "english", "finnish", "french",
               "french", "german", "german", "hungarian", "italian",
               "norwegian", "portuguese", "russian", "spanish", "swedish")

    names[charmatch(gsub("_.*", "", code), codes)]
}

stopwords <- {
    function(language = "eng") {
        resolved <- tm:::map_ISO_639_2(language)
        lang <- if (is.na(resolved))
            language
        else
            resolved
        readLines(system.file("stopwords", paste(lang, ".dat", sep = ""), package = "tm"), encoding = "UTF-8")
    }
}
