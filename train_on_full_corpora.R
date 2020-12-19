# create dictionary
# dict_size <- 60000
# full_dict60 <- words[1:dict_size,]$feature
# rm(words)

# max_n <- 6
# ngrams <- lapply(1:max_n, nFreq, train.tokens)

library(dplyr)
library(quanteda)
library(data.table)

# Prepare news tokens
news.corpus <-  files2sentences('./data/en_US/en_US.news.txt')
news.tokens <- str2tokens(news.corpus)
rm(news.corpus)
save(news.tokens, file="data/news.tokens")

# Create and save news ngrams 3..6
news_ngrams3 <- nFreq(3, news.tokens)
news_ngrams3 <- removeOOD(news_ngrams3, full_dict50)
news_ngrams3 <- setDT(news_ngrams3)
save(news_ngrams3, file='data/news.ngrams3')
rm(news_ngrams3)
gc()

news_ngrams4 <- nFreq(4, news.tokens)
news_ngrams4 <- removeOOD(news_ngrams4, full_dict50)
news_ngrams4 <- setDT(news_ngrams4)
save(news_ngrams4, file='data/news.ngrams4')
rm(news_ngrams4)
gc()

news_ngrams5 <- nFreq(5, news.tokens)
news_ngrams5 <- removeOOD(news_ngrams5, full_dict50)
news_ngrams5 <- setDT(news_ngrams5)
save(news_ngrams5, file='data/news.ngrams5')
rm(news_ngrams5)
gc()

news_ngrams6 <- nFreq(6, news.tokens)
news_ngrams6 <- removeOOD(news_ngrams6, full_dict50)
news_ngrams6 <- setDT(news_ngrams6)
save(news_ngrams6, file='data/news.ngrams6')
rm(news_ngrams6)
gc()

rm(news.tokens)
gc()

#########

# Prepare news tokens
news.corpus <-  files2sentences('./data/en_US/en_US.news.txt')
news.tokens <- str2tokens(news.corpus)
rm(news.corpus)
save(news.tokens, file="data/news.tokens")

# Create and save news ngrams 3..6
news_ngrams3 <- nFreq(3, news.tokens)
news_ngrams3 <- removeOOD(news_ngrams3, full_dict50)
news_ngrams3 <- setDT(news_ngrams3)
save(news_ngrams3, file='data/news.ngrams3')
rm(news_ngrams3)
gc()

news_ngrams4 <- nFreq(4, news.tokens)
news_ngrams4 <- removeOOD(news_ngrams4, full_dict50)
news_ngrams4 <- setDT(news_ngrams4)
save(news_ngrams4, file='data/news.ngrams4')
rm(news_ngrams4)
gc()

news_ngrams5 <- nFreq(5, news.tokens)
news_ngrams5 <- removeOOD(news_ngrams5, full_dict50)
news_ngrams5 <- setDT(news_ngrams5)
save(news_ngrams5, file='data/news.ngrams5')
rm(news_ngrams5)
gc()

news_ngrams6 <- nFreq(6, news.tokens)
news_ngrams6 <- removeOOD(news_ngrams6, full_dict50)
news_ngrams6 <- setDT(news_ngrams6)
save(news_ngrams6, file='data/news.ngrams6')
rm(news_ngrams6)
gc()

rm(news.tokens)
gc()


