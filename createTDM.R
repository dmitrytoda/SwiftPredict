library(tm)
library(RWeka)

createTDM <- function(text, n=1){
        
        # tutorial on rweka - http://tm.r-forge.r-project.org/faq.html
        
        corpus <- VCorpus(VectorSource(text)) # create corpus for TM processing
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, removeNumbers) 
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, stripWhitespace)
        # corpus <- tm_map(corpus, removeWords, stopwords("english")) 
        options(mc.cores=1) # http://stackoverflow.com/questions/17703553/bigrams-instead-of-single-words-in-termdocument-matrix-using-r-and-rweka/20251039#20251039
        my_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n)) # create n-grams
        tdm <- TermDocumentMatrix(corpus, control = list(tokenize = my_tokenizer)) # create tdm from n-grams
        tdm
}

text <- readLines('./data/en_US/sample.twitter.txt')
Bi_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2)) # create n-grams
tdm <- TermDocumentMatrix(corpus, control = list(tokenize = Bi_tokenizer)) # create tdm from n-grams
