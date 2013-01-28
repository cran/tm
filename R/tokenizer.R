getTokenizers <-
function()
    c("MC_tokenizer", "scan_tokenizer")

# http://www.cs.utexas.edu/users/dml/software/mc/
MC_tokenizer <-
function(x)
{
    ASCII_letters <- "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    id <- sprintf("[%s]+", ASCII_letters)
    http <- sprintf("(http://%s(\\.%s)*)", id, id)
    email <- sprintf("(%s@%s(\\.%s)*)", id, id, id)
    http_or_email <- sprintf("%s|%s", http, email)

    c(unlist(regmatches(x, gregexpr(http_or_email, x))),
      unlist(strsplit(gsub(http_or_email, "", x),
                      sprintf("[^%s]", ASCII_letters))))
}

scan_tokenizer <-
function(x)
{
    scan(text = x, what = "character", quote = "", quiet = TRUE)
}
