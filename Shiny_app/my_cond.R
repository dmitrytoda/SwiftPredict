my_cond <- function(ngram) {
        cond <- ".("
        varname <- deparse(substitute(ngram))
        for (i in 1:length(ngram)) cond <- paste0(cond, varname, "[", i, "],")
        substr(cond, nchar(cond), nchar(cond)) <- ")"
        parse(text=cond)
}