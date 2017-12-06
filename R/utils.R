## Helper functions

.print_via_format <-
function(x, ...)
{
    writeLines(format(x, ...))
    invisible(x)
}

## Efficient alternative to table() proposed by Kurt Hornik
.table <- function(x) {
    u <- sort(unique(x))
    if(!length(u)) return(integer())
    v <- tabulate(match(x, u))
    names(v) <- u
    v
}

.xml_content <- function(doc, spec) {
    switch(spec[[1]],
           node = xml_text(xml_find_all(doc, spec[[2]])),
           "function" = spec[[2]](doc),
           unevaluated = spec[[2]])
}

IETF_Snowball_map <-
list("danish" = c("da", "dan"),
     "dutch" = c("nl", "nld", "dut"),
     "english" = c("en", "eng"),
     "finnish" = c("fi", "fin"),
     "french" = c("fr", "fra", "fre"),
     "german" = c("de", "deu", "ger"),
     "hungarian" = c("hu", "hun"),
     "italian" = c("it", "ita"),
     "norwegian" = c("no", "nor"),
     "portuguese" = c("pt", "por"),
     "romanian" = c("ro", "ron", "rum"),
     "russian" = c("ru", "rus"),
     "spanish" = c("es", "esl", "spa"),
     "swedish" = c("sv", "swe"),
     ## Have stopwords but no SnowballC stemmer ...
     "catalan" = c("ca", "cat"),
     ## Have SnowballC stemmer but no stopwords ...
     "turkish" = c("tr", "tur")
     )

# Map IETF language tags to languages used by the Snowball stemmer project
# http://en.wikipedia.org/wiki/IETF_language_tag
map_IETF_Snowball <-
local({
    codes <- unlist(IETF_Snowball_map, use.names = FALSE)
    names <- rep.int(names(IETF_Snowball_map), lengths(IETF_Snowball_map))

    function(code) {
        code <- as.character(code)

        if (identical(code, "") || identical(code, character(0)) || is.na(code))
            return("porter")

        names[charmatch(gsub("-.*", "", code), codes)]
    }
})
