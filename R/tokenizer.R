getTokenizers <- function()
    c("MC_tokenizer", "scan_tokenizer")

# http://www.cs.utexas.edu/users/dml/software/mc/
MC_tokenizer <- function(x) {
    ASCII_letters <- "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    id <- sprintf("[%s]+", ASCII_letters)
    http <- sprintf("(http://%s(\\.%s)*)", id, id)
    email <- sprintf("(%s@%s(\\.%s)*)", id, id, id)
    http_or_email <- sprintf("%s|%s", http, email)

    extract <- function(x, regex) {
        unlist(mapply(function(u, v) {
            mapply(function(start, stop) {
                if (start != -1)
                    substr(u, start, stop)
            }, v, v + attr(v, "match.length") - 1)
        }, x, gregexpr(regex, x), USE.NAMES = FALSE))
    }

    c(extract(x, http_or_email),
      unlist(strsplit(gsub(http_or_email, "", x),
                      sprintf("[^%s]", ASCII_letters))))
}

scan_tokenizer <- function(x) {
    con <- textConnection(x)
    tokens <- scan(con, what = "character", quiet = TRUE)
    close(con)
    tokens
}
