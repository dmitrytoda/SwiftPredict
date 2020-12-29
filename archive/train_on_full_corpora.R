# Batch approach overview:
#         
#         1. Create dictionary:
#         a. Load all three sources (blogs, Twitter, news) together
# b. Create a quanteda corpus, split into sentences
# c. Tokenize
# d. Calculate word frequencies
# e. Select top 20,000 for the dictionary
# f. Remove everything else from memory
# 2. For each of the three sources separately:
#         a. Create a quanteda corpus, split into sentences
# b. Tokenize
# c. For n from 1 to 6:
#         + Calculate n-grams
# + Remove OOD words
# + Save to disk
# + Remove from memory
# d. Remove tokens from memory
# 3. For n from 1 to 4:
#         a. Load ngrams from all sources together
# b. rbind three frequency tables
# c. Collapse equivalent ngrams
# d. Save to disk
# e. Remove from memory
# 4. For n from 5 to 6, as these ngram tables are too heavy to be processed at once:
#         a. Lo



# create dictionary
# dict_size <- 60000
# full_dict60 <- words[1:dict_size,]$feature
# rm(words)

# max_n <- 6
# ngrams <- lapply(1:max_n, nFreq, train.tokens)

library(dplyr)
library(quanteda)
library(data.table)

rm(twitter.tokens)
gc()

# Prepare blogs tokens
blogs.corpus <-  files2sentences('./data/en_US/en_US.blogs.txt')
blogs.tokens <- str2tokens(blogs.corpus)
rm(blogs.corpus)
save(blogs.tokens, file="data/blogs.tokens")

# Create and save blogs ngrams 3..6
blogs_ngrams3 <- nFreq(3, blogs.tokens)
blogs_ngrams3 <- removeOOD(blogs_ngrams3, full_dict50)
blogs_ngrams3 <- setDT(blogs_ngrams3)
save(blogs_ngrams3, file='data/blogs.ngrams3')
rm(blogs_ngrams3)
gc()

blogs_ngrams4 <- nFreq(4, blogs.tokens)
blogs_ngrams4 <- removeOOD(blogs_ngrams4, full_dict50)
blogs_ngrams4 <- setDT(blogs_ngrams4)
save(blogs_ngrams4, file='data/blogs.ngrams4')
rm(blogs_ngrams4)
gc()

####

gc(full = TRUE)
load("Data/blogs.ngrams5 UNSPLIT")

# blogs_ngrams5 <- nFreq(5, blogs.tokens)
blogs_ngrams5 <- removeOOD(blogs_ngrams5, full_dict50)
# blogs_ngrams5 <- setDT(blogs_ngrams5)
save(blogs_ngrams5, file='data/blogs.ngrams5')
rm(blogs_ngrams5)

gc(full = TRUE)

####

Sys.time()
load("Data/blogs.ngrams6 UNSPLIT")

blogs_ngrams6_1of2 <- blogs_ngrams6[1:12688371]
blogs_ngrams6_2of2 <- blogs_ngrams6[12688372:25376741]
save(blogs_ngrams6_1of2, file='Data/blogs_ngrams6_1of2 UNSPLIT')
save(blogs_ngrams6_2of2, file='Data/blogs_ngrams6_2of2 UNSPLIT')

rm(blogs_ngrams6, blogs_ngrams6_2of2)

gc(full=TRUE)
blogs_ngrams6_1of2 <- removeOOD(blogs_ngrams6_1of2, full_dict50)
save(blogs_ngrams6_1of2, file='data/blogs.ngrams6_1of2')

rm(blogs_ngrams6_1of2)
gc(full = TRUE)
load('Data/blogs_ngrams6_2of2 UNSPLIT')
blogs_ngrams6_2of2 <- removeOOD(blogs_ngrams6_2of2, full_dict50)
save(blogs_ngrams6_2of2, file='data/blogs.ngrams6_2of2')



#########

# Prepare news tokens
news.corpus <-  files2sentences('./data/en_US/en_US.news.txt')
news.tokens <- str2tokens(news.corpus)
rm(news.corpus)
save(news.tokens, file="data/news.tokens")

rm(blogs_ngrams6_1of2)
gc(full=TRUE)
load("Data/news.tokens")

# Create and save news ngrams 3..6
news_ngrams3 <- nFreq(3, news.tokens)
save(news_ngrams3, file='data/news.ngrams3 UNSPLIT')
news_ngrams3 <- removeOOD(news_ngrams3, full_dict50)
save(news_ngrams3, file='data/news.ngrams3')
rm(news_ngrams3)
gc(full=TRUE)

news_ngrams4 <- nFreq(4, news.tokens)
save(news_ngrams4, file='data/news.ngrams4 UNSPLIT')
news_ngrams4 <- removeOOD(news_ngrams4, full_dict50)
save(news_ngrams4, file='data/news.ngrams4')
rm(news_ngrams4)
gc(full=TRUE)

news_ngrams5 <- nFreq(5, news.tokens)
save(news_ngrams5, file='data/news.ngrams5 UNSPLIT')
###

load("data/news.ngrams5")
news_ngrams5 <- removeOOD(news_ngrams5, full_dict50)
save(news_ngrams5, file='data/news.ngrams5')
rm(news_ngrams5)
gc(full=TRUE)

news_ngrams6 <- nFreq(6, news.tokens)
save(news_ngrams6, file='data/news.ngrams6 UNSPLIT')

load('data/news.ngrams6 UNSPLIT')

news_ngrams6_1of2 <- news_ngrams6[1:11619912]
news_ngrams6_2of2 <- news_ngrams6[11619913:23239823]
save(news_ngrams6_1of2, file='Data/news_ngrams6_1of2 UNSPLIT')
save(news_ngrams6_2of2, file='Data/news_ngrams6_2of2 UNSPLIT')
rm(news_ngrams6_2of2, news_ngrams6)
gc(full=TRUE)


news_ngrams6_1of2 <- removeOOD(news_ngrams6_1of2, full_dict50)
save(news_ngrams6_1of2, file='data/news.ngrams6_1of2')
rm(news_ngrams6_1of2)
gc(full=TRUE)

load("Data/news_ngrams6_2of2 UNSPLIT")
news_ngrams6_2of2 <- removeOOD(news_ngrams6_2of2, full_dict50)
save(news_ngrams6_2of2, file='data/news.ngrams6_2of2')

rm(news.tokens)
gc(full=TRUE)

###########

## rbind separate files together and collapse repeated ngrams

# 3 grams
load('Data/twitter.ngrams3')
load('Data/blogs.ngrams3')
load('Data/news.ngrams3')
ngrams3 <- rbind(tw_ngrams3, blogs_ngrams3, news_ngrams3)
rm(tw_ngrams3, blogs_ngrams3, news_ngrams3)
gc(full=TRUE)
nrow(ngrams3)
Sys.time()
ngrams3 <- collapse_ngrams(ngrams3)
Sys.time()
nrow(ngrams3)
save(ngrams3, file='Data/ngrams3')
rm(ngrams3)
gc(full=TRUE)

# 4 grams
load('Data/twitter.ngrams4')
load('Data/blogs.ngrams4')
load('Data/news.ngrams4')
ngrams4 <- rbind(tw_ngrams4, blogs_ngrams4, news_ngrams4)
rm(tw_ngrams4, blogs_ngrams4, news_ngrams4)
gc(full=TRUE)
nrow(ngrams4)
Sys.time()
ngrams4 <- collapse_ngrams(ngrams4)
Sys.time()
nrow(ngrams4)
save(ngrams4, file='Data/ngrams4')
rm(ngrams4)
gc(full=TRUE)

# 5 grams
load('Data/twitter.ngrams5')
load('Data/blogs.ngrams5')
load('Data/news.ngrams5')
ngrams5 <- rbind(tw_ngrams5, blogs_ngrams5, news_ngrams5)
rm(tw_ngrams5, blogs_ngrams5, news_ngrams5)
gc(full=TRUE)

load('Data/ngrams5')
ngrams5_1of2 <- ngrams5[1:31793484]
ngrams5_2of2 <- ngrams5[31793485:63586967]
save(ngrams5_1of2, file='data/ngrams5_1of2 DUPL')
save(ngrams5_2of2, file='data/ngrams5_2of2 DUPL')
rm(ngrams5, ngrams5_2of2)
gc(full=TRUE)

nrow(ngrams5_1of2)
Sys.time()
ngrams5_1of2 <- collapse_ngrams(ngrams5_1of2)
Sys.time()
nrow(ngrams5_1of2)
save(ngrams5_1of2, file='Data/ngrams5_1of2')
rm(ngrams5_1of2)
gc(full=TRUE)

load('data/ngrams5_2of2 DUPL')
nrow(ngrams5_2of2)
Sys.time()
ngrams5_2of2 <- collapse_ngrams(ngrams5_2of2)
Sys.time()
nrow(ngrams5_2of2)
save(ngrams5_2of2, file='Data/ngrams5_2of2')

load('data/ngrams5_1of2')
load('data/ngrams5_2of2')

ngrams5 <- ngrams5_1of2
rm(ngrams5_1of2)
gc(full=TRUE)

ngrams5 <- rbindlist(list(ngrams5, ngrams5_2of2))
rm(ngrams5_2of2)
gc(full=TRUE)
save(ngrams5, file='data/ngrams5 DUPL menos')

load('data/ngrams5 DUPL menos')

nrow(ngrams5)
Sys.time()
ngrams5 <- collapse_ngrams(ngrams5)
Sys.time()
nrow(ngrams5)
save(ngrams5, file='data/ngrams5')
rm(ngrams5)
gc(full=TRUE)

load('data/twitter.ngrams6')
load('data/blogs.ngrams6_1of2')
load('data/blogs.ngrams6_2of2')
load('data/news.ngrams6_1of2')
load('data/news.ngrams6_2of2')
ngrams6 <- rbindlist(list(tw_ngrams6, 
                          news_ngrams6_1of2, news_ngrams6_2of2,
                          blogs_ngrams6_1of2, blogs_ngrams6_2of2))
rm(tw_ngrams6, 
   news_ngrams6_1of2, news_ngrams6_2of2,
   blogs_ngrams6_1of2, blogs_ngrams6_2of2)
gc(full=TRUE)
save(ngrams6, file='data/ngrams6 DUPL')

nrow(ngrams6)
Sys.time()
ngrams6 <- collapse_ngrams(ngrams6)
nrow(ngrams6)
Sys.time()
save(ngrams6, file='data/ngrams6')