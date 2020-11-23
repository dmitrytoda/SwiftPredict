createSample <- function(infile, outfile, lines=50000, block=100){
        # Creates a smaller sample file based on a large input file
        # by reading random blocks of lines from the input
        # Arguments:
        # infile, outfile - names of input and output files
        # lines - number of lines in the resulting file
        # block - size of a block to be read from infile
        
        in.con <- file(infile, 'r')
        file.length <- length(readLines(in.con))
        close(in.con)
        
        in.con <- file(infile, 'r')
        out.con <- file(outfile, 'w')
        
        in.blocks <- as.integer(file.length/block)
        out.blocks <- as.integer(lines/block)
        keep.blocks <- sort(sample(1:in.blocks, out.blocks))

        i_prev <- 1
        print(keep.blocks)
        for(i in keep.blocks) {
                #print(paste('i =', i, 'skip =', block*(i-i_prev)))
                tmp <- scan(in.con, what='character', nmax=block, skip = block*(i-i_prev), sep='\n')
                #tmp <- scan(in.con, what='character', sep='\n')
                write(tmp, out.con, append=TRUE, sep='\n')
                #print(tmp)
                i_prev <- i
        }
        
        close(in.con)
        close(out.con)
}


# Create ~50,000 line samples for each of the en_US original files
createSample('./data/en_US/en_US.twitter.txt', './data/en_US/sample.twitter.txt')
createSample('./data/en_US/en_US.blogs.txt', './data/en_US/sample.blogs.txt')
createSample('./data/en_US/en_US.news.txt', './data/en_US/sample.news.txt')

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
        # then may have some more letters, digits, ampersand & or dash - 
        my_tokenizer <- as.Token_Tokenizer(Regexp_Tokenizer("[a-zA-Z0-9]+[a-zA-Z0-9&-']*"))
        sapply(text, my_tokenizer)
}

tmp <- file2tokens('./data/en_US/sample.twitter.txt')

