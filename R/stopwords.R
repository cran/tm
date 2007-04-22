stopwords <- local({
    danish <- readLines(system.file("stopwords", "danish.dat", package = "tm"))
    dutch <- readLines(system.file("stopwords", "dutch.dat", package = "tm"))
    english <- readLines(system.file("stopwords", "english.dat", package = "tm"))
    finnish <- readLines(system.file("stopwords", "finnish.dat", package = "tm"))
    french <- readLines(system.file("stopwords", "french.dat", package = "tm"))
    german <- readLines(system.file("stopwords", "german.dat", package = "tm"))
    hungarian <- readLines(system.file("stopwords", "hungarian.dat", package = "tm"))
    italian <- readLines(system.file("stopwords", "italian.dat", package = "tm"))
    norwegian <- readLines(system.file("stopwords", "norwegian.dat", package = "tm"))
    portuguese <- readLines(system.file("stopwords", "portuguese.dat", package = "tm"))
    russian <- readLines(system.file("stopwords", "russian.dat", package = "tm"))
    spanish <- readLines(system.file("stopwords", "spanish.dat", package = "tm"))
    swedish <- readLines(system.file("stopwords", "swedish.dat", package = "tm"))
    function(language = "en") {
        resolved <- resolveISOCode(language)
        lang <- if (is.null(resolved))
            language
        else
            resolved
        get(lang)
    }
})
