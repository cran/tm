# .accessor_method_words_table <- NULL
#
# .match_accessor_method_words <- function(x) {
#    if(is.na(pos <- pmatch(x, .accessor_method_words_table)))
#      stop("Unknown accessor method for words()")
#    get(.accessor_method_words_table[pos])
# }
#
# words <- function(x, ...) UseMethod("words", x)
# words.Corpus <- function(x) {
#     FUN <- meta(x, "accessors", type = "corpus")$words
#     if (is.character(FUN))
#         FUN <- .match_accessor_method_words(FUN)
#     else if (!is.function(FUN))
#         FUN <- words
#     lapply(x, FUN)
# }
# words.PlainTextDocument <- function(x, ...) {
#     unique(scan_tokenizer(x))
# }
