clusterAvailable <- function() {
    clActive() && suppressWarnings(require("snow", quietly = TRUE)) && suppressWarnings(require("Rmpi", quietly = TRUE)) && !is.null(snow::getMPIcluster())
}
