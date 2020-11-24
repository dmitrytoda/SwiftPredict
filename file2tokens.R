library(tm)

file2tokens <- function (infile) {
        # Reads ALL lines from infile and converts to a list of 
        # char vectors, each element of a vector representing a token
        
        con <- file(infile, 'r')
        text <- readLines(con)
        close(con)
        
        # remove empty strings (artifact of sampling)
        text <- text[text!='']
        
        # A token = starts with a letter or a digit
        # then may have some more letters, digits, or &-'’ symbols
        # alternatively, acronyms like U.S. are also tokens
        my_tokenizer <- as.Token_Tokenizer(Regexp_Tokenizer(
                "[a-zA-Z0-9]+[a-zA-Z0-9&-'’]*|([a-zA-Z]\\.){2,}"
                ))
        sapply(text, my_tokenizer)
}



