setMethod("show",
          signature(object = "PlainTextDocument"),
          function(object){
              cat(noquote(Content(object)), sep = "\n")
    })

print.MinimalDocument <- function(x, ...)
    cat(noquote(as.character(x)), sep = "\n")
