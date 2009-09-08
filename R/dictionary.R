# Author: Ingo Feinerer

Dictionary <- function(x) UseMethod("Dictionary", x)
Dictionary.character <- function(x)
    structure(x, class = c("Dictionary", "character"))
Dictionary.TermDocumentMatrix <- Dictionary.DocumentTermMatrix <- function(x)
    structure(Terms(x), class = c("Dictionary", "character"))
