# Author: Ingo Feinerer

dissimilarity <- function(x, y = NULL, method) UseMethod("dissimilarity", x)

dissimilarity.TermDocumentMatrix <- function(x, y = NULL, method)
    proxy::dist(as.matrix(t(x)), y, method)

dissimilarity.DocumentTermMatrix <- function(x, y = NULL, method)
    proxy::dist(as.matrix(x), y, method)

dissimilarity.PlainTextDocument <- function(x, y = NULL, method) {
    tdm <- TermDocumentMatrix(c(x, y))
    dissimilarity(tdm, method = method)
}
