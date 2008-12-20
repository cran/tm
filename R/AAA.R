clActive <- local({
    # Default is to NOT use a cluster if available
    cla <- FALSE
    function(new, ...) {
        if (!missing(new))
            cla <<- new
        else
            cla
    }
})

activateCluster <- function() clActive(TRUE)

deactivateCluster <- function() clActive(FALSE)

.onLoad <- function(libname, pkgname) {
    require("methods")
    if (suppressWarnings(require("snow", quietly = TRUE)) &&
        suppressWarnings(require("Rmpi", quietly = TRUE)) &&
        tryCatch(is.null(snow::getMPIcluster()), error = function(e) FALSE))
        try(snow::makeMPIcluster(Rmpi::mpi.universe.size()), silent = TRUE)
}

.Last <- function() {
    if (suppressWarnings(require("snow", quietly = TRUE)) && suppressWarnings(require("Rmpi", quietly = TRUE)))
        try(snow::stopCluster(snow::getMPIcluster()), silent = TRUE)
}
