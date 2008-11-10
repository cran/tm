plot.TermDocMatrix <- function(x,
                               terms = sample(colnames(x), 20),
                               corThreshold = 0.7,
                               weighting = FALSE,
                               attrs = list(graph = list(rankdir = "BT"),
                                            node = list(shape = "rectangle",
                                                        fixedsize = FALSE)),
                               ...) {
    if (!require("Rgraphviz"))
        stop("could not find (bioconductor.org) Rgraphviz package")

    c <- cor(as.matrix(x[seq_len(nrow(x)), terms]))
    c[c < corThreshold] <- 0
    diag(c) <- 0
    g <- as(c, "graphNEL")
    p <- plot(g, attrs = attrs, ...)
    if (weighting) {
        i <- 1
        lw <- round(c[lower.tri(c) & c >= corThreshold] * 10)
        for (ae in Rgraphviz::AgEdge(p)) {
            lines(ae, lwd = lw[i], len = 1)
            i <- i + 1
        }
    }
    invisible(p)
}
