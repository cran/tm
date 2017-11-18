getTokenizers <-
function()
    c("Boost_tokenizer", "MC_tokenizer", "scan_tokenizer")

# http://www.boost.org
Boost_tokenizer <-
Token_Tokenizer(function(x) enc2utf8(Boost_Tokenizer(as.character(x))))

# http://www.cs.utexas.edu/users/dml/software/mc/
MC_tokenizer <-
Token_Tokenizer(function(x)
{
    x <- as.character(x)
    ASCII_letters <- "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    id <- sprintf("[%s]+", ASCII_letters)
    http <- sprintf("(http://%s(\\.%s)*)", id, id)
    email <- sprintf("(%s@%s(\\.%s)*)", id, id, id)
    http_or_email <- sprintf("%s|%s", http, email)

    c(unlist(regmatches(x, gregexpr(http_or_email, x))),
      unlist(strsplit(gsub(http_or_email, "", x),
                      sprintf("[^%s]", ASCII_letters))))
})

scan_tokenizer <-
Token_Tokenizer(function(x) .Call(`_tm_scan`, x, 0L))
