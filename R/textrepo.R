# Author: Ingo Feinerer

TextRepository <- function(x, meta = list(created = as.POSIXlt(Sys.time(), tz = "GMT"))) {
    x <- structure(list(x), class = c("TextRepository", "list"))
    attr(x, "RepoMetaData") <- meta
    x
}

RepoMetaData <- function(x) attr(x, "RepoMetaData")

print.TextRepository <- function(x, ...) {
    cat(sprintf(ngettext(length(x),
                         "A text repository with %d corpus\n",
                         "A text repository with %d corpora\n"),
                length(x)))
    invisible(x)
}

summary.TextRepository <- function(object, ...) {
    print(object)
    if (length(RepoMetaData(object))) {
        cat(sprintf(ngettext(length(RepoMetaData(object)),
                             "\nThe repository metadata consists of %d tag-value pair\n",
                             "\nThe repository metadata consists of %d tag-value pairs\n"),
                    length(RepoMetaData(object))))
        cat("Available tags are:\n")
        cat(names(RepoMetaData(object)), "\n")
    }
}
