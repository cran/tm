## A simple class for sparse (triplet) matrices.

## Mostly intended for being able to take advantage of LP solvers which
## allow for sparse specifictions of (possible rather large) constraint
## matrices.

simple_triplet_matrix <-
function(i, j, v, nrow = max(i), ncol = max(j), dimnames = NULL)
{
    structure(list(i = i, j = j, v = v, nrow = nrow, ncol = ncol,
                   dimnames = dimnames),
              class = "simple_triplet_matrix")
}

as.simple_triplet_matrix <-
function(x)
    UseMethod("as.simple_triplet_matrix")

as.simple_triplet_matrix.simple_triplet_matrix <- identity

as.simple_triplet_matrix.matrix <-
function(x)
{
    if(!prod(dim(x)))
        return(simple_triplet_matrix(integer(), integer(), c(x),
                                     nrow = nrow(x), ncol = ncol(x),
                                     dimnames = dimnames(x)))
    ind <- which(x != vector(typeof(x), 1L), arr.ind = TRUE)
    dimnames(ind) <- NULL
    simple_triplet_matrix(ind[, 1L], ind[, 2L], x[ind],
                          nrow = nrow(x), ncol = ncol(x),
                          dimnames = dimnames(x))
}

as.matrix.simple_triplet_matrix <-
function(x, ...)
{
    nr <- x$nrow
    nc <- x$ncol
    y <- matrix(vector(typeof(x$v), nr * nc), nr, nc)
    y[cbind(x$i, x$j)] <- x$v
    dimnames(y) <- x$dimnames
    y
}

## We could also simply write a method to coerce to a dgTMatrix, based
## on something like
##  new("dgTMatrix",
##       i = as.integer(i - 1),
##       j = as.integer(j - 1),
##       x = v,
##       Dim = c(nrow, ncol))
## (Note that these have C-style indices starting at zero.)

Ops.simple_triplet_matrix <-
function(e1, e2)
{
    ## Currently, we only implement binary addition of two simple
    ## triplet matrices, and multiplication and division of a simple
    ## triplet matrix by a number.
    ## More could be added (but note that the elements could have
    ## arbitrary modes).
    
    if(nargs() == 1L)
        stop(gettextf("Unary '%s' not defined for \"%s\" objects.",
                      .Generic, .Class))

    if(!(as.character(.Generic) %in% c("+", "*", "/")))
        stop(gettextf("Generic '%s' not defined for \"%s\" objects.",
                      .Generic, .Class))

    ## Obviously, the following could be generalized ...
    if(as.character(.Generic) == "*") {
        if(length(e1) == 1L) {
            e2$v <- e2$v * e1
            return(e2)
        }
        if(length(e2) == 1L) {
            e1$v <- e1$v * e2
            return(e1)
        }
        stop("Not implemented.")
    }
    if(as.character(.Generic) == "/") {
        if(length(e2) == 1L) {
            e1$v <- e1$v / e2
            return(e1)
        }
        stop("Not implemented.")
    }

    ## This leaves adding two simple triplet matrices.
    e1 <- as.simple_triplet_matrix(e1)
    e2 <- as.simple_triplet_matrix(e2)
    ## Check dimensions: currently, no recycling.
    if((e1$nrow != e2$nrow) || (e1$ncol != e2$ncol))
        stop("Incompatible dimensions.")
    if(length(e1$v) < length(e2$v)) {
        ## Swap e1 and e2 so that duplicated indices can be found more
        ## efficiently.
        e3 <- e1
        e1 <- e2
        e2 <- e3
    }
    ## Find duplicated indices.
    pos <- match(paste(e2$i, e2$j, sep = "\r"),
                 paste(e1$i, e1$j, sep = "\r"),
                 nomatch = 0L)
    ind <- which(pos == 0L)
    e1$v[pos] <- e1$v[pos] + e2$v[pos > 0L]
    e1$i <- c(e1$i, e2$i[ind])
    e1$j <- c(e1$j, e2$j[ind])
    e1$v <- c(e1$v, e2$v[ind])
    ## Maybe do some more about dimnames eventually.
    e1
}

dim.simple_triplet_matrix <-
function(x)
    c(x$nrow, x$ncol)

## <TODO>
## Add a dim setter.
## </TODO>

dimnames.simple_triplet_matrix <-
function(x)
    x$dimnames

`dimnames<-.simple_triplet_matrix` <-
function(x, value)
{
    if(!is.null(value)) {
        ## Should be a list of length 2.
        if(!is.list(value) || (length(value) != 2L))
            stop("Invalid dimnames.")
        ind <- sapply(value, length) == 0L
        if(all(ind))
            value <- NULL
        else {
            dnx <- vector("list", 2L)
            dnx[!ind] <- lapply(value[!ind], as.character)
        }
    }
    if(is.null(value))
        x["dimnames"] <- list(NULL)
    else
        x$dimnames <- dnx
    x
}

`[.simple_triplet_matrix` <-
function(x, i, j, ...)
{
    ## (Well, we certainly don't drop ...)

    ## Note that calling x[] with a simple triplet matrix x will call
    ## the subscript method with args x and missing ...
    na <- nargs()
    if((na == 1L) || (na == 2L) && missing(i))
        return(x)

    nr <- x$nrow
    nc <- x$ncol    

    if(na == 2L) {
        ## Single index subscripting.
        if(is.logical(i))
            stop("Logical subscripting currently not implemented.")
        else if(is.character(i))
            stop("Character subscripting currently not implemented.")
        else if(!is.matrix(i)) {
            ## Let's hope we have a vector.
            ## What if we have both negatives and positives?
            if(all(i >= 0)) {
                i <- i[i > 0]
                out <- vector(mode = typeof(x$v), length = length(i))
                pos <- match(i, (x$j - 1L) * nr + x$i, 0)
                out[pos > 0] <- x$v[pos]
            } else if(all(i <= 0)) {
                out <- vector(mode = typeof(x$v), nr * nc)
                out[(x$j - 1L) * nr + x$i] <- x$v
                out <- out[i]
            }
            else stop("Cannot mix positive and negative subscripts.")
        }
        else {
            ## Note that negative values are not allowed in a matrix
            ## subscript.
            if((ncol(i) != 2L) || (any(i < 0)))
                stop("Invalid subscript.")
            i <- i[!apply(i == 0, 1L, any), , drop = FALSE]
            out <- vector(mode = typeof(x$v), length = nrow(i))
            ##  pi <- match(i[, 1L], x$i)
            ##  pj <- match(i[, 2L], x$j)
            ##  ind <- which(pi == pj)
            ##  out[ind] <- x$v[pi[ind]]
            pos <- match(paste(i[,1L], i[,2L], sep = "\r"),
                         paste(x$i, x$j, sep = "\r"),
                         nomatch = 0L)
            out[pos > 0] <- x$v[pos]
        }
    }
    else {
        ## Two index subscripting is rather tricky, as it can also be
        ## used for rearranging and "recycling" rows and columns.  Let
        ## us not support the latter for now, so that selected rows and
        ## columns must be unique.
        if(missing(i)) {
            pos <- rep.int(TRUE, length(x$v))
            pi <- seq_len(nr)
        }
        else if(!is.numeric(i))
            stop("Only numeric two-index subscripting is implemented.")
        else {
            pi <- seq_len(nr)
            if(all(i >= 0)) {
                i <- i[i > 0]
                if(any(duplicated(i)))
                    stop("Repeated indices currently not allowed.")
            } else if(all(i <= 0))
                i <- pi[i]
            else
                stop("Cannot mix positive and negative subscripts.")
            nr <- length(i)
            pos <- match(x$i, i, 0) > 0
            pi[i] <- seq_len(nr)
        }
        if(missing(j)) {
            pj <- seq_len(nc)
        }
        else if(!is.numeric(j))
            stop("Only numeric two-index subscripting is implemented.")
        else {
            pj <- seq_len(nc)
            if(all(j >= 0)) {
                j <- j[j > 0]
                if(any(duplicated(j)))
                    stop("Repeated indices currently not allowed.")
            } else if(all(j <= 0))
                j <- pj[j]
            else
                stop("Cannot mix positive and negative subscripts.")
            nc <- length(j)
            pos <- pos & (match(x$j, j, 0) > 0)            
            pj[j] <- seq_len(nc)
        }
        if(!is.null(dnx <- x$dimnames))
            dnx <- list(dnx[[1L]][i], dnx[[2L]][j])
        
        out <- simple_triplet_matrix(pi[x$i[pos]], pj[x$j[pos]],
                                     x$v[pos], nr, nc, dnx)
    }
            
    out
}

rbind.simple_triplet_matrix <-
function(..., deparse.level = 1L)
{
    args <- lapply(Filter(Negate(is.null), list(...)),
                   as.simple_triplet_matrix)
    ## Ignore 'deparse.level' ...
    out <- Reduce(function(x, y) {
        if((nc <- ncol(x)) != ncol(y))
            stop("Numbers of columns of matrices must match.")
        nr <- nrow(x)
        simple_triplet_matrix(c(x$i, y$i + nr),
                              c(x$j, y$j),
                              c(x$v, y$v),
                              nrow = nr + nrow(y), ncol = nc)
    }, args)
    ## Handle dimnames in one final step.
    rnms <- lapply(args, rownames)
    rnms <- if(!any(sapply(rnms, is.null)))
        do.call("c", rnms)
    else
        NULL
    cnms <- Find(Negate(is.null), lapply(args, colnames))
    dimnames(out) <- list(rnms, cnms)
    out
}

cbind.simple_triplet_matrix <-
function(..., deparse.level = 1L)
{
    args <- lapply(Filter(Negate(is.null), list(...)),
                   as.simple_triplet_matrix)
    ## Ignore 'deparse.level' ...
    out <- Reduce(function(x, y) {
        if((nr <- nrow(x)) != nrow(y))
            stop("Numbers of rows of matrices must match.")
        nc <- ncol(x)
        simple_triplet_matrix(c(x$i, y$i),
                              c(x$j, y$j + nc),
                              c(x$v, y$v),
                              nrow = nr, ncol = nc + ncol(y))
    }, args)
    ## Handle dimnames in one final step.
    rnms <- Find(Negate(is.null), lapply(args, rownames))
    cnms <- lapply(args, rownames)
    cnms <- if(!any(sapply(cnms, is.null)))
        do.call("c", cnms)
    else
        NULL
    dimnames(out) <- list(rnms, cnms)
    out
}

t.simple_triplet_matrix <-
function(x)
    simple_triplet_matrix(x$j, x$i, x$v, x$ncol, x$nrow, rev(x$dimnames))

duplicated.simple_triplet_matrix <-
function(x, incomparables = FALSE, MARGIN = 1L, fromLast = FALSE, ...)
{
    ## We could use the duplicated method for class matrix, but at the
    ## expense of going from sparse to dense ...
    if(!is.logical(incomparables) || incomparables) 
        .NotYetUsed("incomparables != FALSE")
    if(MARGIN == 1L) {
        i <- x$i
        j <- x$j
        len <- x$nrow
    } else if(MARGIN == 2L) {
        i <- x$j
        j <- x$i
        len <- x$ncol
    } else
        stop("Invalid margin.")    
    o <- order(i, j)
    y <- split(paste(j[o], x$v[o], sep = "\r"), i[o])
    tmp <- character(len)
    names(tmp) <- seq_along(tmp)
    tmp[names(y)] <- sapply(y, paste, collapse = "\r")
    duplicated(tmp, fromLast = fromLast)
}

unique.simple_triplet_matrix <-
function(x, incomparables = FALSE, MARGIN = 1L, fromLast = FALSE, ...)
{
    if(!is.logical(incomparables) || incomparables)
        .NotYetUsed("incomparables != FALSE")
    ind <- !duplicated(x, MARGIN = MARGIN, fromLast = fromLast)
    if(MARGIN == 1L)
        x[which(ind), ]
    else
        x[, which(ind)]
}

c.simple_triplet_matrix <-
function(..., recursive = FALSE)
{
    args <- list(...)
    ind <- which(sapply(args, inherits, "simple_triplet_matrix"))
    args[ind] <-
        lapply(args[ind],
               function(x) {
                   y <- vector(typeof(x$v), x$nrow * x$ncol)
                   y[x$i + (x$j - 1L) * x$nrow] <- x$v
                   y
               })
    do.call(c, args)
}

## Utitilies for creating special simple triplet matrices:

.simple_triplet_zero_matrix <-
function(nrow, ncol = nrow, mode = "double")
    simple_triplet_matrix(integer(), integer(), vector(mode, 0L),
                          nrow, ncol)

.simple_triplet_diag_matrix <-
function(x, nrow)
{
    x <- rep(x, length.out = nrow)
    i <- seq_len(nrow)
    simple_triplet_matrix(i, i, x, nrow, nrow)
}

## A simple class for sparse arrays.

## Not very useful yet: need at least a subscript method.
## (Unfortunately, additional methods such as for rowSums/colSums or
## apply, etc., are not straightforward to add in an S3 world ...)

simple_sparse_array <-
function(i, v, dim = NULL, dimnames = NULL)
{
    ## <FIXME>
    ## Add some sanity checking eventually ...
    ## i should be a matrix of indices (non-"zero" entries).
    ## v should be a "vector" of non-zero values, with length equal to
    ## the number of rows of i.
    ## </FIXME>
    if(is.null(dim)) dim <- apply(i, 2L, max)
    ## <FIXME>
    ## Add checks for dimnames: should be NULL or a list of entries
    ## which are either NULL or character vectors as long as the
    ## corresponding dim.
    ## </FIXME>
    structure(list(i = i, v = v, dim = dim, dimnames = dimnames),
              class = "simple_sparse_array")
}

as.simple_sparse_array <-
function(x)
    UseMethod("as.simple_sparse_array")

as.simple_sparse_array.simple_sparse_array <- identity

as.simple_sparse_array.array <-
function(x)
{
    if(!prod(dim(x)))
        simple_sparse_array(array(integer(), dim(x)), c(x),
                            dim(x), dimnames(x))
    ind <- which(x != vector(typeof(x), 1L), arr.ind = TRUE)
    dimnames(ind) <- NULL
    simple_sparse_array(ind, x[ind], dim(x), dimnames(x))
}

as.array.simple_sparse_array <-
function(x, ...)
{
    v <- x$v
    dim <- x$dim
    y <- array(vector(typeof(x), prod(dim)), dim = dim,
               dimnames = x$dimnames)
    y[x$i] <- v
    y
}

dim.simple_sparse_array <-
function(x)
    x$dim

dimnames.simple_sparse_array <-
function(x)
    x$dimnames

## <TODO>
## Add dim and dimnames setters.
## </TODO>

`[.simple_sparse_array` <-
function(x, ...)
{
    ## Note that calling x[] with a simple sparse array x will call the
    ## subscript method with args x and missing ...
    na <- nargs()
    if((na == 1L) || (na == 2L) && missing(..1))
        return(x)

    nd <- length(x$dim)
    spos <- function(i) {
        ## Scalar positions of array index matrices i in the usual row
        ## major ordering of arrays.
        cpd <- cumprod(x$dim)
        1L + rowSums((i - 1L) * rep(c(1L, cpd[-nd]), each = cpd[nd]))
    }

    if(na == 2L) {
        i <- ..1
        ## Single index subscripting.
        if(is.logical(i))
            stop("Logical subscripting currently not implemented.")
        else if(is.character(i))
            stop("Character subscripting currently not implemented.")
        else if(!is.matrix(i)) {
            ## Let's hope we have a vector.
            ## What if we have both negatives and positives?
            if(all(i >= 0)) {
                i <- i[i > 0]
                out <- vector(mode = typeof(x$v), length = length(i))
                pos <- match(i, spos(x$i), 0)
                out[pos > 0] <- x$v[pos]
            } else if(all(i <= 0)) {
                out <- vector(mode = typeof(x$v), prod(x$dim))
                out[spos(x$i)] <- x$v
                out <- out[i]
            }
            else stop("Cannot mix positive and negative subscripts.")
        }
        else {
            ## Note that negative values are not allowed in a matrix
            ## subscript.
            if((ncol(i) != nd) || (any(i < 0)))
                stop("Invalid subscript.")
            i <- i[!apply(i == 0, 1L, any), , drop = FALSE]
            out <- vector(mode = typeof(x$v), length = nrow(i))
            ## This is not really the fastest way to match rows, but is
            ## there an obvious better one?
            pos <- match(split(i, row(i)), split(x$i, row(x$i)))
            out[pos > 0] <- x$v[pos]
        }
    }
    else {
        if(na != (nd + 1L))
            stop("Incorrect number of dimensions.")
        ## Figure out the missing arguments (if any).
        args <- substitute(list(...))
        ## Replace missing arguments by NULL for now.
        args[sapply(args,
                    function(a)
                    (length(a) == 1L) && (as.character(a) == ""))] <-
                        list(NULL)
        ## (Could also test args for identical(as.character(a), "").)
        ## And evaluate.
        args <- eval(args)
        ## Ready to go.
        dx <- x$dim
        pos <- rep.int(TRUE, length(x$v))
        ind <- lapply(dx, seq_len)
        for(k in seq_len(nd)) {
            i <- args[[k]]              # Given indices.
            if(is.null(i)) next
            else if(!is.numeric(i))
                stop("Only numeric multi-index subscripting is implemented.")
            else {
                if(all(i >= 0)) {
                    i <- i[i > 0]
                    if(any(duplicated(i)))
                        stop("Repeated indices currently not allowed.")
                } else if(all(i <= 0))
                    i <- seq_len(dx[k])[i]
                else
                    stop("Cannot mix positive and negative subscripts.")
                ind[[k]] <- i
                dx[k] <- length(i)
                j <- match(x$i[, k], i, 0L)
                x$i[j > 0L, k] <- seq_along(i)[j]
                pos <- pos & (j > 0L)
            }
        }
        if(!is.null(dnx <- x$dimnames))
            dnx <- Map("[", dnx, ind)
        out <- simple_sparse_array(x$i[pos, , drop = FALSE], x$v[pos],
                                   dx, dnx)
    }

    out
    
}

## <TODO>
## Add duplicated and unique methods for simple sparse arrays along the
## lines of the corresponding methods for simple triplet matrices.
## </TODO>
