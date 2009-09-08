# Author: Ingo Feinerer

# Preprocess the Reuters21578 XML data
preprocessReut21578XML <- function(input, output, fixEnc = TRUE) {
    dir.create(output, recursive = TRUE)
    files <- dir(input, pattern = "\\.xml", full.names = TRUE)

    if (fixEnc) {
        # Correct invalid UTF-8 encoding
        # The invalid multibyte string is in reut2-017.xml at line 35578
        content <- readLines(paste(input, "reut2-017.xml", sep = ""))
        content[35578] <- "world economic growth. side measures to boost growth, he said."
        writeLines(content, paste(input, "reut2-017.xml", sep = ""))
    }

    # Write out each article in a seperate file
    counter <- 1
    for (f in files) {
        tree <- XML::xmlTreeParse(f)
        XML::xmlApply(XML::xmlRoot(tree),
                 function(article) {
                     output.file <- file.path(output,
                                              sprintf("reut-%s.xml", formatC(counter, width = 5, flag = "0")))
                     counter <<- counter + 1
                     con <- file(output.file, "w")
                     XML::saveXML(article, file = con)
                     close(con)
                 })
    }
}
