resolveISOCode <- function(code) {
    switch(gsub("_.*", "", code),
           "da" = "danish",
           "de" = "german",
           "en" = "english",
           "es" = "spanish",
           "fi" = "finnish",
           "fr" = "french",
           "hu" = "hungarian",
           "it" = "italian",
           "nl" = "dutch",
           "no" = "norwegian",
           "pt" = "portuguese",
           "ru" = "russian",
           "sv" = "swedish")
}
