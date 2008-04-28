## Helper functions

.xml_value_if_not_null <- function(n, default) if (!is.null(n)) XML::xmlValue(n) else default

.xml_content <- function(doc, spec) {
    type <- spec[[1]]
    fun <- switch(type,
                  node = XML::xmlValue,
                  attribute = identity)

    if (identical(type, "unevaluated"))
        spec[[2]]
    else if (identical(type, "function") && is.function(spec[[2]]))
        spec[[2]](doc)
    else
        as.character(sapply(XML::getNodeSet(doc, spec[[2]]), fun))
}
