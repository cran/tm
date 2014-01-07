stopwords <- {
    function(kind = "en") {
        stopifnot(is.character(kind))

        resolved <- map_IETF_Snowball(kind)
        base <- if (is.na(resolved))
            kind
        else if (identical(resolved, "porter"))
            "english"
        else
            resolved
        s <- system.file("stopwords", paste(base, ".dat", sep = ""), package = "tm")
        if (identical(s, ""))
            stop(paste("no stopwords available for '", base, "'", sep = ""))
        readLines(s, encoding = "UTF-8")
    }
}
