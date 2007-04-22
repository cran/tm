setMethod("show",
          signature(object = "PlainTextDocument"),
          function(object){
              print(Corpus(object))
    })
