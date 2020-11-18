# Author: Ingo Feinerer
# Filters

tm_filter <-
function(x, FUN, ...)
    UseMethod("tm_filter", x)
tm_filter.PCorpus <-
tm_filter.SimpleCorpus <-
tm_filter.VCorpus <-
function(x, FUN, ...)
    x[tm_index(x, FUN, ...)]

tm_index <-
function(x, FUN, ...)
    UseMethod("tm_index", x)
tm_index.PCorpus <-
tm_index.SimpleCorpus <-
tm_index.VCorpus <-
function(x, FUN, ...)
    unlist(tm_parLapply(content(x), function(y) isTRUE(FUN(y, ...))))
