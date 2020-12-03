# input: vector of file names
# output: quanteda corpus reshaped to sentences

files2sentences <- function(filenames) {
        texts <- sapply(filenames, readLines, skipNul = TRUE)
        texts <- sapply(texts, enc2utf8)
        texts <- unname(unlist(texts))
        my_corpus <- corpus(texts)
        corpus_reshape(my_corpus, to="sentences")
        
}
