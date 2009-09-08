# Author: Ingo Feinerer

stemCompletion <- function(x, words, type = c("prevalent", "first")) {
    # Get a list of all terms from the corpus
    terms <- unlist(lapply(x, strsplit, "[^[:alnum:]]+"))

    type <- match.arg(type)
    switch(type,
           # As heuristics just take the first found completion
           first = na.omit(sapply(words, function(w) grep(sprintf("^%s", w), unique(terms), value = TRUE)[1])),
           # As heuristics take the most frequent completion
           prevalent = {
               possibleCompletions <- lapply(words, function(w) grep(sprintf("^%s", w), terms, value = TRUE))
               possibleCompletions <- lapply(possibleCompletions, table)
               possibleCompletions <- lapply(possibleCompletions, sort, decreasing = TRUE)
               s <- structure(names(sapply(possibleCompletions, "[", 1)), names = words)
               s[nchar(s) > 0]}
           )
}
