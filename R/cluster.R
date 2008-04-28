clActive <- local({
    cla <- FALSE
    function(new, ...) {
        if (!missing(new))
            cla <<- new
        else
            cla
    }
})

clusterAvailable <- function() clActive()

activateCluster <- function() {
    if (require("snow") && require("Rmpi") && is.null(snow::getMPIcluster()))
        snow::makeMPIcluster(Rmpi::mpi.universe.size())
    if (!is.null(snow::getMPIcluster()))
        clActive(TRUE)
}

deactivateCluster <- function() {
    clActive(FALSE)
    snow::stopCluster(snow::getMPIcluster())
}
