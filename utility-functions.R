library('slam')

download_data <- function() {
    
    DATA_URL <- 'https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/'
    FILE_NAME <- 'Coursera-SwiftKey.zip'
    
    if (!file.exists('data')) {
        dir.create('data')
    }
    
    if (!file.exists('data/final')) {
        temp_file <- tempfile()
        download.file(paste0(DATA_URL, FILE_NAME), temp_file)
        unzip(temp_file, exdir = 'data')
        unlink(temp_file)
    }
    
}

read_data <- function(fraction) {
    
    read_file <- function(file_name) {
        con <- file(file_name, open = 'r')
        on.exit(close(con))
        readLines(con, encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
    }
    
    get_subset <- function (text) 
    {
        length <- length(text)
        sample(text, size = length * fraction, replace = FALSE)
    }
    
    dir <- './data/final/en_US/'
    blogs_file <- paste0(dir, 'en_US.blogs.txt')
    news_file <- paste0(dir, 'en_US.news.txt')
    twitter_file <- paste0(dir, 'en_US.twitter.txt')
    
    blogs <- read_file(blogs_file)
    news <- read_file(news_file)
    twitter <- read_file(twitter_file)

    blogs_subset <- get_subset(blogs)
    news_subset <- get_subset(news)
    twitter_subset <- get_subset(twitter)
    
    en_subset <- c(blogs_subset, news_subset, twitter_subset)
    
    # clear up memory
    rm(blogs, news, twitter)
    rm(blogs_subset, news_subset, twitter_subset)
    
    return(en_subset)
}


clean_data <- function(text)
    {
    
    # split into sentences
    text <- unlist(sapply(text, function(x) unlist(strsplit(x,"[?!\\.]"))), 
                   use.names = FALSE)
    text <- text[text != ""]
    
    library('tm')
    cps <- VCorpus(VectorSource(text))
    
    removePattern <- content_transformer(function(x, pattern) gsub(pattern, "", x))
    
    addEndSymbols <- content_transformer(
        function(x) paste('<s>', x))
    
    
    
    cps <- tm_map(cps, removePattern, "(f|ht)tp(s?)://\\S+")
    cps <- tm_map(cps, removePattern, 
                        "[[:alnum:]\\.-]+@[[:alnum:].[:alnum:]]+")
    cps <- tm_map(cps, removePattern, "@\\S+")
    cps <- tm_map(cps, removePattern, "#\\S+")
    cps <- tm_map(cps, removePattern, "-")
    cps <- tm_map(cps, removePattern, "\"")
    cps <- tm_map(cps, content_transformer(removePunctuation))
    cps <- tm_map(cps, content_transformer(removeNumbers))
    cps <- tm_map(cps, content_transformer(tolower))
    cps <- tm_map(cps, content_transformer(stripWhitespace))
    cps <- tm_map(cps, removePattern, "^\\s+") # remove leading whitespace
    
    # add sentence start and sentence finisher
    cps <- tm_map(cps, addEndSymbols)
    
    
    # download bad word file
    URL <- 'https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en'
    if (!file.exists('data/bad_words.csv')) {
        download.file(URL, 'data/bad_words.csv')
    }
    bad_words <- readLines('./data/bad_words.csv')
    
    # remove bad words
    removeBadWords <- content_transformer(function(x) removeWords(x, bad_words))
    cps <- tm_map(cps, removeBadWords)
    
}


compare <- function(text, corpus, line) {
    print(text[line])
    print(corpus[[line]]$content)
}


get_unigram_counts <- function(cps) {
    library('slam')
    UnigramTokenizer <- 
        function(x) unlist(words(x))
    # TermDocumentMatri discards words fewer than 3 characters by default...
    unigram_tdm <- TermDocumentMatrix(cps, 
                                      control = list(wordLengths = c(1, Inf)))
    row_sums(unigram_tdm)
}


get_bigram_counts <- function(cps) {
    BigramTokenizer <- 
        function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), 
                           use.names = FALSE)
    bigram_tdm <- TermDocumentMatrix(cps, 
                                     control = list(tokenize = BigramTokenizer))
    row_sums(bigram_tdm)
}

get_trigram_counts <- function(cps) {
    TrigramTokenizer <- 
        function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), 
                           use.names = FALSE)
    trigram_tdm <- TermDocumentMatrix(cps, 
                                     control = list(tokenize = TrigramTokenizer))
    row_sums(trigram_tdm)
}

get_quadrigram_counts <- function(cps) {
    QuadrigramTokenizer <- 
        function(x) unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), 
                           use.names = FALSE)
    quadrigram_tdm <- TermDocumentMatrix(cps, 
                                      control = list(tokenize = QuadrigramTokenizer))
    row_sums(quadrigram_tdm)
}

get_quintigram_counts <- function(cps) {
    QuintigramTokenizer <- 
        function(x) unlist(lapply(ngrams(words(x), 5), paste, collapse = " "), 
                           use.names = FALSE)
    quintigram_tdm <- TermDocumentMatrix(cps, 
                                         control = list(tokenize = QuintigramTokenizer))
    row_sums(quintigram_tdm)
}

