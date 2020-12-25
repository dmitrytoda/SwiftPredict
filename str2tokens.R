# converts a string into tokens
# res=tokens returns tokens for further processing
# res=vector returns a character vector to make predictions upon
str2tokens <- function(string, res='tokens', dict) {
        require(quanteda)
        stopifnot(res %in% c('tokens', 'vector'))
        my_tokens <- tokens(
                string,
                what = "word",
                remove_punct = TRUE,
                remove_symbols = TRUE,
                remove_numbers = TRUE,
                remove_url = TRUE,
                remove_separators = TRUE,
                split_hyphens = FALSE,
                include_docvars = FALSE,
                padding = FALSE
        )
        
        # keep only tokens that contain at least one letter
        my_tokens <- tokens_select(
                my_tokens, 
                "[a-z]+", 
                valuetype = "regex", 
                selection = "keep",  
                case_insensitive = TRUE)
        
        # remove tokens that contain weird characters,
        # i.e. anything but letters, digits and #'.-’ signs
        my_tokens <- tokens_select(
                my_tokens, 
                "[^\\#a-z0-9\\'\\.\\-]+", 
                valuetype = "regex", 
                selection = "remove",  
                case_insensitive = TRUE)
        
        if(res=='tokens') return(my_tokens)
        
        res <- tolower(my_tokens[[1]])
        res[! res %in% dict] <- '<UNK>'
        res
}
