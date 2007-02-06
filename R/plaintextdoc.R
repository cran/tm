setMethod("show",
          signature(object = "PlainTextDocument"),
          function(object){
              print(Content(object))
    })
