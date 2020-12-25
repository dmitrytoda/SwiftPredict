stupid_predict('i love', my_model, full_dict50)

my_model[[1]]$rank <- as.factor(c(1,2,3))
my_model[[1]]$frequency <- NULL

my_model[[2]][,rank:=.N:1,by=X1]
my_model[[2]]$frequency <- NULL
my_model[[2]]$rank <- as.factor(my_model[[2]]$rank)

my_model[[3]][,rank:=.N:1, by=.(X1, X2)]
my_model[[3]]$frequency <- NULL
my_model[[3]]$rank <- as.factor(my_model[[3]]$rank)

my_model[[4]][,rank:=.N:1, by=.(X1, X2, X3)]
my_model[[4]]$frequency <- NULL
my_model[[4]]$rank <- as.factor(my_model[[4]]$rank)

my_model[[5]][,rank:=.N:1, by=.(X1, X2, X3, X4)]
my_model[[5]]$frequency <- NULL
my_model[[5]]$rank <- as.factor(my_model[[5]]$rank)

my_model[[6]][,rank:=.N:1, by=.(X1, X2, X3, X4, X5)]
my_model[[6]]$frequency <- NULL
my_model[[6]]$rank <- as.factor(my_model[[6]]$rank)

setkey(my_model[[1]], rank)
setkey(my_model[[2]], X1, rank)
setkey(my_model[[3]], X1, X2, rank)
setkey(my_model[[4]], X1, X2, X3, rank)
setkey(my_model[[5]], X1, X2, X3, X4, rank)
setkey(my_model[[6]], X1, X2, X3, X4, X5, rank)
