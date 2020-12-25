test_dict <- c('a', 'b', 'c', 'd', 'e')


n1 <- read.csv('test/n1.csv')
n2 <- read.csv('test/n2.csv')
n3 <- read.csv('test/n3.csv')

test_model <- list(n1, n2, n3)
test_model <- lapply(test_model, setDT)

t1 <- lapply(test_model, shrink_ngrams, dict20, 1)
