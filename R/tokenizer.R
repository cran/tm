scan_tokenizer <- function(x) {
    con <- textConnection(x)
    tokens <- scan(con, what = "character", quiet = TRUE)
    close(con)
    tokens
}
