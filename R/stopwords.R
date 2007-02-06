stopwords <- {
    function(language = "en") {
        resolved <- tm:::resolveISOCode(language)
        lang <- if (is.null(resolved))
            language
        else
            resolved
        readLines(system.file("stopwords", paste(lang, ".dat", sep = ""), package = "tm"), encoding = "UTF-8")
    }
}
