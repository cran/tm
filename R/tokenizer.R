getTokenizers <-
function()
    c("Boost_tokenizer", "MC_tokenizer", "scan_tokenizer")

## <http://www.boost.org>
Boost_tokenizer <-
Token_Tokenizer(function(x)
{
    y <- Boost_Tokenizer(as.character(x))
    Encoding(y) <- "UTF-8"
    y
})

## <http://www.cs.utexas.edu/users/dml/software/mc/>
MC_tokenizer <-
Token_Tokenizer(function(x)
{
    x <- as.character(x)
    if(!length(x))
        return(character())
    ASCII_letters <- "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    id <- sprintf("[%s]+", ASCII_letters)
    http <- sprintf("(https?://%s(\\.%s)*)", id, id)
    email <- sprintf("(%s@%s(\\.%s)*)", id, id, id)
    http_or_email <- sprintf("%s|%s", http, email)

    y <- c(unlist(regmatches(x, gregexpr(http_or_email, x)),
                  FALSE, FALSE),
           unlist(strsplit(gsub(http_or_email, "", x),
                           sprintf("[^%s]", ASCII_letters)),
                  FALSE, FALSE))
    y[nzchar(y)]
})

scan_tokenizer <-
Token_Tokenizer(function(x)
{
    .Call(`_tm_scan`, as.character(x), 0L)
})
