### R code from vignette source 'tm.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: Init
###################################################
library("tm")
data("crude")


###################################################
### code chunk number 2: Ovid
###################################################
txt <- system.file("texts", "txt", package = "tm")
(ovid <- Corpus(DirSource(txt, encoding = "UTF-8"),
                readerControl = list(language = "lat")))


###################################################
### code chunk number 3: VectorSource
###################################################
docs <- c("This is a text.", "This another one.")
Corpus(VectorSource(docs))


###################################################
### code chunk number 4: Reuters
###################################################
reut21578 <- system.file("texts", "crude", package = "tm")
reuters <- Corpus(DirSource(reut21578),
                  readerControl = list(reader = readReut21578XML))


###################################################
### code chunk number 5: tm.Rnw:120-121 (eval = FALSE)
###################################################
## writeCorpus(ovid)


###################################################
### code chunk number 6: tm.Rnw:132-133
###################################################
inspect(ovid[1:2])


###################################################
### code chunk number 7: tm.Rnw:137-138
###################################################
identical(ovid[[2]], ovid[["ovid_2.txt"]])


###################################################
### code chunk number 8: tm.Rnw:156-157
###################################################
reuters <- tm_map(reuters, as.PlainTextDocument)


###################################################
### code chunk number 9: tm.Rnw:165-166
###################################################
reuters <- tm_map(reuters, stripWhitespace)


###################################################
### code chunk number 10: tm.Rnw:171-172
###################################################
reuters <- tm_map(reuters, tolower)


###################################################
### code chunk number 11: Stopwords
###################################################
reuters <- tm_map(reuters, removeWords, stopwords("english"))


###################################################
### code chunk number 12: Stemming
###################################################
tm_map(reuters, stemDocument)


###################################################
### code chunk number 13: tm.Rnw:204-206
###################################################
query <- "id == '237' & heading == 'INDONESIA SEEN AT CROSSROADS OVER ECONOMIC CHANGE'"
tm_filter(reuters, FUN = sFilter, query)


###################################################
### code chunk number 14: DublinCore
###################################################
DublinCore(crude[[1]], "Creator") <- "Ano Nymous"
meta(crude[[1]])


###################################################
### code chunk number 15: tm.Rnw:237-241
###################################################
meta(crude, tag = "test", type = "corpus") <- "test meta"
meta(crude, type = "corpus")
meta(crude, "foo") <- letters[1:20]
meta(crude)


###################################################
### code chunk number 16: tm.Rnw:258-260
###################################################
dtm <- DocumentTermMatrix(reuters)
inspect(dtm[1:5,100:105])


###################################################
### code chunk number 17: tm.Rnw:269-270
###################################################
findFreqTerms(dtm, 5)


###################################################
### code chunk number 18: tm.Rnw:275-276
###################################################
findAssocs(dtm, "opec", 0.8)


###################################################
### code chunk number 19: tm.Rnw:288-289
###################################################
inspect(removeSparseTerms(dtm, 0.4))


###################################################
### code chunk number 20: tm.Rnw:303-305
###################################################
inspect(DocumentTermMatrix(reuters,
                           list(dictionary = c("prices", "crude", "oil"))))


