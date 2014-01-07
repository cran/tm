### R code from vignette source 'extensions.Rnw'

###################################################
### code chunk number 1: Init
###################################################
library("tm")
library("XML")


###################################################
### code chunk number 2: extensions.Rnw:71-76
###################################################
VecSource <- function(x) {
    s <- Source(length = length(x), names = names(x), class = "VectorSource")
    s$Content <- as.character(x)
    s
}


###################################################
### code chunk number 3: extensions.Rnw:85-89
###################################################
getElem.VectorSource <-
function(x) list(content = x$Content[x$Position], uri = NA)
pGetElem.VectorSource <-
function(x) lapply(x$Content, function(y) list(content = y, uri = NA))


###################################################
### code chunk number 4: extensions.Rnw:114-117
###################################################
readPlain <-
function(elem, language, id)
    PlainTextDocument(elem$content, id = id, language = language)


###################################################
### code chunk number 5: extensions.Rnw:145-150
###################################################
df <- data.frame(contents = c("content 1", "content 2", "content 3"),
                 title    = c("title 1"  , "title 2"  , "title 3"  ),
                 authors  = c("author 1" , "author 2" , "author 3" ),
                 topics   = c("topic 1"  , "topic 2"  , "topic 3"  ),
                 stringsAsFactors = FALSE)


###################################################
### code chunk number 6: extensions.Rnw:156-157
###################################################
names(attributes(PlainTextDocument()))


###################################################
### code chunk number 7: Mapping
###################################################
m <- list(Content = "contents", Heading = "title",
          Author = "authors", Topic = "topics")


###################################################
### code chunk number 8: myReader
###################################################
myReader <- readTabular(mapping = m)


###################################################
### code chunk number 9: extensions.Rnw:180-181
###################################################
(corpus <- Corpus(DataframeSource(df), readerControl = list(reader = myReader)))


###################################################
### code chunk number 10: extensions.Rnw:186-188
###################################################
corpus[[1]]
meta(corpus[[1]])


###################################################
### code chunk number 11: CustomXMLFile
###################################################
custom.xml <- system.file("texts", "custom.xml", package = "tm")
print(readLines(custom.xml), quote = FALSE)


###################################################
### code chunk number 12: mySource
###################################################
mySource <- function(x, encoding = "UTF-8")
    XMLSource(x, function(tree) XML::xmlChildren(XML::xmlRoot(tree)), myXMLReader, encoding)


###################################################
### code chunk number 13: myXMLReader
###################################################
myXMLReader <- readXML(
    spec = list(Author = list("node", "/document/writer"),
                Content = list("node", "/document/description"),
                DateTimeStamp = list("function",
                                     function(x) as.POSIXlt(Sys.time(), tz = "GMT")),
                Description = list("attribute", "/document/@short"),
                Heading = list("node", "/document/caption"),
                ID = list("function", function(x) tempfile()),
                Origin = list("unevaluated", "My private bibliography"),
                Type = list("node", "/document/type")),
    doc = PlainTextDocument())


###################################################
### code chunk number 14: extensions.Rnw:301-302
###################################################
corpus <- Corpus(mySource(custom.xml))


###################################################
### code chunk number 15: extensions.Rnw:306-308
###################################################
corpus[[1]]
meta(corpus[[1]])


