# Author: Ingo Feinerer

WeightFunction <- function(x, name, acronym) {
    class(x) <- c("WeightFunction", "function")
    attr(x, "Name") <- name
    attr(x, "Acronym") <- acronym
    x
}

# Actual TermDocumentMatrix weighting functions
weightTf <-
    WeightFunction(function(m) {
        attr(m, "Weighting") <- c("term frequency", "tf")
        m
    }, "term frequency", "tf")

weightTfIdf <-
    WeightFunction(function(m, normalize = TRUE) {
        isDTM <- inherits(m, "DocumentTermMatrix")
        if (isDTM) m <- t(m)
        if (normalize) {
            cs <- col_sums(m)
            if (any(cs == 0))
                warning("empty document")
            names(cs) <- seq_len(nDocs(m))
            m$v <- m$v / cs[m$j]
        }
        rs <- row_sums(m > 0)
        if (any(rs == 0))
            warning("term does not occur in the corpus")
        m <- m * log2(nDocs(m) / rs)
        attr(m, "Weighting") <- c(sprintf("%s%s",
                                          "term frequency - inverse document frequency",
                                          if (normalize) " (normalized)" else ""),
                                  "tf-idf")
        if (isDTM) t(m) else m
    }, "term frequency - inverse document frequency", "tf-idf")

# Not yet finished
weightSMART <-
    WeightFunction(function(m, spec = "nnn", control = list()) {
        if (nchar(spec) != 3)
            stop("invalid spec")

        term_frequency <- match.arg(substr(spec, 1, 1), c("n", "l", "a", "b", "L"))
        document_frequency <- match.arg(substr(spec, 2, 2), c("n", "t", "p"))
        normalization <- match.arg(substr(spec, 3, 3), c("n", "b"))

        isDTM <- inherits(m, "DocumentTermMatrix")
        if (isDTM) m <- t(m)

        # Term frequency
        m$v <- switch(term_frequency,
                      # natural
                      n = m$v,
                      # logarithm
                      l = 1 + log2(m$v),
                      # augmented
                      a = 0.5 + (0.5 * m$v) / tapply(m$v, m$j, max),
                      # boolean
                      b = rep(1, length(m$v)),
                      # log ave
                      # TODO: What does avg_{t \in d} in Manning et al. mean?
                      L = (1 + log2(m$v)) / (1 + log2(tapply(m$v, m$j, mean))))

        # Document frequency
        rs <- row_sums(m > 0)
        if (any(rs == 0))
            warning("term does not occur in the corpus")
        df <- switch(document_frequency,
                     # natural
                     n = 1,
                     # idf
                     t = log2(nDocs(m) / rs),
                     # prob idf
                     p = max(0, log2((nDocs(m) - rs) / rs)))

        # Normalization
        norm <- switch(normalization,
                       # none
                       n = rep(1, nDocs(m)),
                       # cosine
                       # Not implemented yet.
                       # pivoted unique
                       # Not implemented yet.
                       # byte size
                       b = {
                           if (is.null(control$charlength))
                               stop("invalid control argument charlength")
                           if (is.null(control$charlength))
                               stop("invalid control argument alpha")
                           control$charlength^control$alpha
                       })

        m <- m * df
        names(norm) <- seq_len(nDocs(m))
        m$v <- m$v / norm[m$j]
        attr(m, "Weighting") <- c(paste("SMART", spec), "SMART")

        if (isDTM) t(m) else m
    }, "SMART", "SMART")

weightBin <-
    WeightFunction(function(m) {
        m$v <- rep(1, length(m$v))
        attr(m, "Weighting") <- c("binary", "bin")
        m
    }, "binary", "bin")
