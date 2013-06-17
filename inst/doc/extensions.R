### R code from vignette source 'extensions.Rnw'

###################################################
### code chunk number 1: Init
###################################################
library("tm")
library("XML")


###################################################
### code chunk number 2: extensions.Rnw:53-58
###################################################
df <- data.frame(contents = c("content 1", "content 2", "content 3"),
                 title    = c("title 1"  , "title 2"  , "title 3"  ),
                 authors  = c("author 1" , "author 2" , "author 3" ),
                 topics   = c("topic 1"  , "topic 2"  , "topic 3"  ),
                 stringsAsFactors = FALSE)


###################################################
### code chunk number 3: extensions.Rnw:64-65
###################################################
names(attributes(PlainTextDocument()))


###################################################
### code chunk number 4: Mapping
###################################################
m <- list(Content = "contents", Heading = "title",
          Author = "authors", Topic = "topics")


###################################################
### code chunk number 5: myReader
###################################################
myReader <- readTabular(mapping = m)


###################################################
### code chunk number 6: extensions.Rnw:88-89
###################################################
(corpus <- Corpus(DataframeSource(df), readerControl = list(reader = myReader)))


###################################################
### code chunk number 7: extensions.Rnw:94-96
###################################################
corpus[[1]]
meta(corpus[[1]])


###################################################
### code chunk number 8: CustomXMLFile
###################################################
custom.xml <- system.file("texts", "custom.xml", package = "tm")
print(readLines(custom.xml), quote = FALSE)


###################################################
### code chunk number 9: mySource
###################################################
mySource <- function(x, encoding = "UTF-8")
    XMLSource(x, function(tree) XML::xmlChildren(XML::xmlRoot(tree)), myXMLReader, encoding)


###################################################
### code chunk number 10: myXMLReader
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
### code chunk number 11: extensions.Rnw:209-210
###################################################
corpus <- Corpus(mySource(custom.xml))


###################################################
### code chunk number 12: extensions.Rnw:214-216
###################################################
corpus[[1]]
meta(corpus[[1]])


