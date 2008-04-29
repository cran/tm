setMethod("show",
          signature(object = "PlainTextDocument"),
          function(object){
              print(noquote(Content(object)))
    })
