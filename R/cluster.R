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

tm_startCluster <- function() {
    if (require("snow") && require("Rmpi") && is.null(snow::getMPIcluster()))
        snow::makeMPIcluster(Rmpi::mpi.universe.size())
    if (!is.null(snow::getMPIcluster()))
        clActive(TRUE)
}

tm_stopCluster <- function() {
    clActive(FALSE)
    snow::stopCluster(snow::getMPIcluster())
}
