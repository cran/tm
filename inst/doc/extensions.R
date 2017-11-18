### R code from vignette source 'extensions.Rnw'

###################################################
### code chunk number 1: Init
###################################################
library("tm")
library("xml2")


###################################################
### code chunk number 2: extensions.Rnw:55-58
###################################################
VecSource <- function(x)
    SimpleSource(length = length(x), content = as.character(x),
                 class = "VecSource")


###################################################
### code chunk number 3: extensions.Rnw:68-72
###################################################
getElem.VecSource <-
function(x) list(content = x$content[x$position], uri = NULL)
pGetElem.VecSource <-
function(x) lapply(x$content, function(y) list(content = y, uri = NULL))


###################################################
### code chunk number 4: extensions.Rnw:100-102
###################################################
readPlain <- function(elem, language, id)
    PlainTextDocument(elem$content, id = id, language = language)


###################################################
### code chunk number 5: extensions.Rnw:124-130
###################################################
df <- data.frame(doc_id   = c("doc 1"    , "doc 2"    , "doc 3"    ),
		 text     = c("content 1", "content 2", "content 3"),
                 title    = c("title 1"  , "title 2"  , "title 3"  ),
                 authors  = c("author 1" , "author 2" , "author 3" ),
                 topics   = c("topic 1"  , "topic 2"  , "topic 3"  ),
                 stringsAsFactors = FALSE)


###################################################
### code chunk number 6: extensions.Rnw:138-141
###################################################
(corpus <- Corpus(DataframeSource(df)))
corpus[[1]]
meta(corpus[[1]])


###################################################
### code chunk number 7: CustomXMLFile
###################################################
custom.xml <- system.file("texts", "custom.xml", package = "tm")
print(readLines(custom.xml), quote = FALSE)


###################################################
### code chunk number 8: mySource
###################################################
mySource <- function(x)
    XMLSource(x, parser = xml2::xml_children, reader = myXMLReader)


###################################################
### code chunk number 9: myXMLReader
###################################################
myXMLReader <- readXML(
    spec = list(author = list("node", "writer"),
                content = list("node", "description"),
                datetimestamp = list("function",
                    function(x) as.POSIXlt(Sys.time(), tz = "GMT")),
                description = list("node", "@short"),
                heading = list("node", "caption"),
                id = list("function", function(x) tempfile()),
                origin = list("unevaluated", "My private bibliography"),
                type = list("node", "type")),
    doc = PlainTextDocument())


###################################################
### code chunk number 10: extensions.Rnw:244-245
###################################################
corpus <- VCorpus(mySource(custom.xml))


###################################################
### code chunk number 11: extensions.Rnw:249-251
###################################################
corpus[[1]]
meta(corpus[[1]])


