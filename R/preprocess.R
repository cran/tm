# Author: Ingo Feinerer

# Preprocess the Reuters21578 XML data
preprocessReut21578XML <- function(reuters.dir, reuters.oapf.dir, fix.enc = TRUE) {
    dir.create(reuters.oapf.dir, recursive = TRUE)
    files <- dir(reuters.dir, pattern = "\\.xml", full.names = TRUE)

    if (fix.enc) {
        # Correct invalid UTF-8 encoding
        # The invalid multibyte string is in reut2-017.xml at line 35578
        content <- readLines(paste(reuters.dir, "reut2-017.xml", sep = ""))
        content[35578] <- "world economic growth. side measures to boost growth, he said."
        writeLines(content, paste(reuters.dir, "reut2-017.xml", sep = ""))
    }

    # Write out each article in a seperate file
    counter <- 1
    for (f in files) {
        tree <- xmlTreeParse(f)
        xmlApply(xmlRoot(tree),
                 function(article) {
                     output.file <- paste(reuters.oapf.dir, "reut-",
                                          gsub(" ", "0", format(counter, width = 5)),
                                          ".xml", sep = "")
                     counter <<- counter + 1
                     con <- file(output.file, "w")
                     saveXML(article, file = con)
                     close(con)
                 })
    }
}

convertMboxEml <- function(mbox, eml.dir) {
    dir.create(eml.dir, recursive = TRUE)
    content <- readLines(mbox)
    counter <- start <- end <- 1
    needWrite <- FALSE
    for (i in seq_along(content)) {
        if (length(grep("^From ", content[i])) > 0) {
            end <- i - 1
            if (needWrite && start <= end) {
                con <- file(paste(eml.dir, counter, sep = ""))
                writeLines(content[start:end], con)
                close(con)
                needWrite <- FALSE
                counter <- counter + 1
            }
            start <- i
            needWrite <- TRUE
        }
    }
    if (needWrite && start <= end)
        writeLines(content[start:end], file(paste(eml.dir, counter, sep = "")))
}
