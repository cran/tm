# Author: Ingo Feinerer

stemCompletion <-
function(x, dictionary,
         type = c("prevalent", "first", "longest",
                  "none", "random", "shortest"))
    UseMethod("stemCompletion", x)
stemCompletion.PlainTextDocument <-
function(x, dictionary,
         type = c("prevalent", "first", "longest",
                  "none", "random", "shortest"))
{
    tokens <- scan_tokenizer(x)
    sc <- unlist(lapply(x, function(x)
                        paste(stemCompletion.character(tokens, dictionary, type),
                              collapse = " ")),
                 use.names = FALSE)
    Content(x) <- if (is.character(sc)) sc else ""
    x
}
stemCompletion.character <-
function(x, dictionary,
         type = c("prevalent", "first", "longest",
                  "none", "random", "shortest"))
{
    if (inherits(dictionary, "Corpus"))
        dictionary <- unlist(lapply(dictionary, strsplit, "[^[:alnum:]]+"))

    type <- match.arg(type)
    possibleCompletions <- lapply(x, function(w) grep(sprintf("^%s", w),
                                                      dictionary,
                                                      value = TRUE))
    switch(type,
           first = {
               structure(sapply(possibleCompletions, "[", 1), names = x)
           },
           longest = {
               ordering <-
                   lapply(possibleCompletions,
                          function(x) order(nchar(x),decreasing = TRUE))
               possibleCompletions <-
                   mapply(function(x, id) x[id], possibleCompletions,
                          ordering, SIMPLIFY = FALSE)
               structure(sapply(possibleCompletions, "[", 1), names = x)
           },
           none = {
               structure(x, names = x)
           },
           prevalent = {
               possibleCompletions <-
                   lapply(possibleCompletions,
                          function(x) sort(table(x), decreasing = TRUE))
               n <- names(sapply(possibleCompletions, "[", 1))
               structure(if (length(n)) n else NA, names = x)
           },
           random = {
               structure(sapply(possibleCompletions, function(x) {
                   if (length(x)) sample(x, 1) else NA
               }), names = x)
           },
           shortest = {
               ordering <- lapply(possibleCompletions, function(x) order(nchar(x)))
               possibleCompletions <-
                   mapply(function(x, id) x[id], possibleCompletions,
                          ordering, SIMPLIFY = FALSE)
               structure(sapply(possibleCompletions, "[", 1), names = x)
           }
           )
}
