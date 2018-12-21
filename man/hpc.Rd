\name{hpc}
\alias{tm_parLapply}
\alias{tm_parLapply_engine}
\title{Parallelized \sQuote{lapply}}
\description{
  Parallelize applying a function over a list or vector according to the
  registered parallelization engine.
}
\usage{
tm_parLapply(X, FUN, ...)
tm_parLapply_engine(new)
}
\arguments{
  \item{X}{A vector (atomic or list), or other objects suitable for the
    engine in use.}
  \item{FUN}{the function to be applied to each element of \code{X}.}
  \item{...}{optional arguments to \code{FUN}.}
  \item{new}{an object inheriting from class \code{cluster} as created
    by \code{\link[parallel:makeCluster]{makeCluster}()} from package
    \pkg{parallel}, or a function with formals \code{X}, \code{FUN} and
    \code{...}, or \code{NULL} corresponding to the default of using no
    parallelization engine.}
}
\details{
  Parallelization can be employed to speed up some of the embarrassingly
  parallel computations performed in package \pkg{tm}, specifically
  \code{\link{tm_index}()}, \code{\link{tm_map}()} on a non-lazy-mapped
  \code{\link{VCorpus}}, and \code{\link{TermDocumentMatrix}()} on a
  \code{\link{VCorpus}} or \code{\link{PCorpus}}.

  Functions \code{tm_parLapply()} and \code{tm_parLapply_engine()} can
  be used to customize parallelization according to the available
  resources.

  \code{tm_parLapply_engine()} is used for getting (with no arguments)
  or setting (with argument \code{new}) the parallelization engine
  employed (see below for examples).

  If an engine is set to an object inheriting from class \code{cluster},
  \code{tm_parLapply()} calls
  \code{\link[parallel:parLapply]{parLapply}()} with this cluster and 
  the given arguments.  If set to a function, \code{tm_parLapply()}
  calls the function with the given arguments.  Otherwise, it simply
  calls \code{\link{lapply}()}.

  Hence, parallelization via
  \code{\link[parallel:parLapply]{parLapply}()}
  and a default cluster registered via
  \code{\link[parallel:setDefaultCluster]{setDefaultCluster}()} can be
  achieved via \preformatted{  tm_parLapply_engine(function(X, FUN, ...)
      parallel::parLapply(NULL, X, FUN, ...))}
  or re-registering the cluster, say \code{cl}, using \preformatted{  tm_parLapply_engine(cl)}
  (note that since \R version 3.5.0, one can use
  \code{\link[parallel:getDefaultCluster]{getDefaultCluster}()} to get
  the registered default cluster).  Using \preformatted{  tm_parLapply_engine(function(X, FUN, ...)
      parallel::parLapplyLB(NULL, X, FUN, ...))}
  or \preformatted{  tm_parLapply_engine(function(X, FUN, ...)
      parallel::parLapplyLB(cl, X, FUN, ...))}
  gives load-balancing parallelization with the registered default or
  given cluster, respectively.  To achieve parallelization via forking
  (on Unix-alike platforms), one can use the above with clusters created
  by \code{\link[parallel:makeForkCluster]{makeForkCluster}()}, or use \preformatted{  tm_parLapply_engine(parallel::mclapply)}
  or \preformatted{  tm_parLapply_engine(function(X, FUN, ...)
      parallel::mclapply(X, FUN, ..., mc.cores = n))}
  to use \code{\link[parallel:mclapply]{mclapply}()} with the default or
  given number \code{n} of cores.
}
\value{
  A list the length of \code{X}, with the result of applying \code{FUN}
  together with the \code{...} arguments to each element of \code{X}.
}
\seealso{
  \code{\link[parallel:makeCluster]{makeCluster}()},
  \code{\link[parallel:parLapply]{parLapply}()},
  \code{\link[parallel:parLapplyLB]{parLapplyLB}()}, and
  \code{\link[parallel:mclapply]{mclapply}()}.
}
