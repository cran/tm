tm_tag_score <-
function(x, tags, FUN)
    UseMethod("tm_tag_score", x)
tm_tag_score.term_frequency <-
function(x, tags, FUN)
    FUN(x[match(tags, names(x), nomatch = 0L)])
tm_tag_score.PlainTextDocument <-
function(x, tags, FUN = function(x) sum(x, na.rm = TRUE))
    tm_tag_score(termFreq(x, control = list(removePunctuation = TRUE)),
                 tags, FUN)
tm_tag_score.TermDocumentMatrix <-
function(x, tags, FUN = slam::col_sums)
    FUN(x[match(tags, Terms(x), nomatch = 0L), ])
tm_tag_score.DocumentTermMatrix <-
function(x, tags, FUN = slam::row_sums)
    FUN(x[, match(tags, Terms(x), nomatch = 0L)])
