\name{tm_cluster}
\alias{tm_startCluster}
\alias{tm_stopCluster}
\title{Allow `tm' to Use a Cluster}
\description{
  \pkg{tm} can use a (MPI) cluster if available on your system.
}
\usage{
tm_startCluster()
tm_stopCluster()
}
\details{
  \code{tm_startCluster} first investigates the MPI environment and
  tries to attach to a running MPI instance. If no MPI instance is found
  the function starts a new one. On success \pkg{tm} functions
  automatically use the cluster.

  \code{tm_stopCluster} shuts down a running MPI instance.
}
\author{Ingo Feinerer}
