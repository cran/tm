# Author: Ingo Feinerer

FunctionGenerator <- function(x) {
    class(x) <- c("FunctionGenerator", "function")
    x
}
