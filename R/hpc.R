tm_parLapply_engine <-
local({
    val <- NULL
    ## Could do some checking on new if given: should inherit from
    ## "cluster" or have formals (X, FUN, ...).
    function(new) {
        if (missing(new)) val else val <<- new
    }
})

tm_parLapply <-
function(X, FUN, ...)
{
    engine <- tm_parLapply_engine()
    if (inherits(engine, "cluster"))
        parLapply(engine, X, FUN, ...)
    else if (is.function(engine))
        engine(X, FUN, ...)
    else
        lapply(X, FUN, ...)
}
