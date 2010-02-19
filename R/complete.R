# Author: Ingo Feinerer

stemCompletion <- function(x, words, type = c("prevalent", "first", "longest", "none", "random", "shortest")) {
    terms <- if (inherits(x, "Corpus"))
        unlist(lapply(x, strsplit, "[^[:alnum:]]+"))
    else
        x

    type <- match.arg(type)
    possibleCompletions <- lapply(words, function(w) grep(sprintf("^%s", w), terms, value = TRUE))
    switch(type,
           first = {
               structure(sapply(possibleCompletions, "[", 1), names = words)
           },
           longest = {
               ordering <- lapply(possibleCompletions, function(x) order(nchar(x), decreasing = TRUE))
               possibleCompletions <- mapply(function(x, id) x[id], possibleCompletions, ordering, SIMPLIFY = FALSE)
               structure(sapply(possibleCompletions, "[", 1), names = words)
           },
           none = {
               structure(words, names = words)
           },
           prevalent = {
               possibleCompletions <- lapply(possibleCompletions, function(x) sort(table(x), decreasing = TRUE))
               structure(names(sapply(possibleCompletions, "[", 1)), names = words)
           },
           random = {
               structure(sapply(possibleCompletions, function(x) {
                   if (length(x) >= 1) sample(x, 1)
                   else NA
               }), names = words)
           },
           shortest = {
               ordering <- lapply(possibleCompletions, function(x) order(nchar(x)))
               possibleCompletions <- mapply(function(x, id) x[id], possibleCompletions, ordering, SIMPLIFY = FALSE)
               structure(sapply(possibleCompletions, "[", 1), names = words)
           }
           )
}
