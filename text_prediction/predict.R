load('unigrams.RData')
load('bigrams.RData')
load('trigrams.RData')
load('quadrigrams.RData')
load('quintigrams.RData')
library(tm)
library(dplyr)

clean_input <- function(input){
    input <- removePunctuation(input)
    input <- tolower(input)
    return(input)
}

quintigram_prediction <- function(last_word, second_last_word, third_last_word,
                                  fourth_last_word){
    output <- data.frame(prediction=character(),
                         ngram=numeric(),
                         count=numeric(),
                         type=character())
    df <- quintigrams[quintigrams$first_word == fourth_last_word & quintigrams$second_word == third_last_word & quintigrams$third_word == second_last_word & quintigrams$fourth_word ==last_word,]
    
    df2 <- quintigrams[quintigrams$second_word == fourth_last_word & quintigrams$third_word == third_last_word & quintigrams$fourth_word == second_last_word & startsWith(quintigrams$fifth_word,last_word) & quintigrams$fifth_word !=last_word,]
    
    if (nrow(df) > 0){
        output1 <- data.frame(prediction = paste0(' ',df$fifth_word), 
                              ngram = rep(4,length(df$count)), 
                              count = df$count,
                              type=rep('word', nrow(df)))
        output <- rbind(output,output1)
    }
    if (nrow(df2) > 0){
        summary <- df2 %>% group_by(fifth_word) %>% summarize(total = max(count)) %>% arrange(desc(total))
        summary <- summary[1:min(3,nrow(summary)),]
        output2 <- data.frame(prediction = summary$fifth_word,
                              ngram = rep(5, nrow(summary)),
                              count = summary$total,
                              type = rep('completion', nrow(summary)))
        output <- rbind(output,output2)
    }
    return(output)
}

quadrigram_prediction <- function(last_word, second_last_word, third_last_word){
    output <- data.frame(prediction=character(),
                         ngram=numeric(),
                         count=numeric(),
                         type=character())
    df <- quadrigrams[quadrigrams$first_word == third_last_word & quadrigrams$second_word == second_last_word & quadrigrams$third_word == last_word,]
    
    df2 <- quadrigrams[quadrigrams$second_word == third_last_word & quadrigrams$third_word == second_last_word & startsWith(quadrigrams$fourth_word,last_word) & quadrigrams$fourth_word !=last_word,]
    
    if (nrow(df) > 0){
        output1 <- data.frame(prediction = paste0(' ',df$fourth_word), 
                              ngram = rep(4,length(df$count)), 
                              count = df$count,
                              type=rep('word', nrow(df)))
        output <- rbind(output,output1)
    }
    if (nrow(df2) > 0){
        summary <- df2 %>% group_by(fourth_word) %>% summarize(total = max(count)) %>% arrange(desc(total))
        summary <- summary[1:min(3,nrow(summary)),]
        output2 <- data.frame(prediction = summary$fourth_word,
                              ngram = rep(4, nrow(summary)),
                              count = summary$total,
                              type = rep('completion', nrow(summary)))
        output <- rbind(output,output2)
    }
    return(output)
}

trigram_prediction <- function(last_word, second_last_word){
    output <- data.frame(prediction=character(),
                         ngram=numeric(),
                         count=numeric(),
                         type=character())
    df <- trigrams[trigrams$first_word == second_last_word & trigrams$second_word == last_word,]
    df2 <- trigrams[trigrams$second_word == second_last_word & startsWith(trigrams$third_word,last_word) & trigrams$third_word !=last_word,]
    if (nrow(df) > 0){
        output1 <- data.frame(prediction = paste0(' ',df$third_word), 
                             ngram = rep(3,length(df$count)), 
                             count = df$count,
                             type=rep('word', nrow(df)))
        output <- rbind(output,output1)
    }
    if (nrow(df2) > 0){
        summary <- df2 %>% group_by(third_word) %>% summarize(total = max(count)) %>% arrange(desc(total))
        summary <- summary[1:min(3,nrow(summary)),]
        output2 <- data.frame(prediction = summary$third_word,
                              ngram = rep(3, nrow(summary)),
                              count = summary$total,
                              type = rep('completion', nrow(summary)))
        output <- rbind(output,output2)
    }
    return(output)
}

bigram_prediction <- function(last_word){
    output <- data.frame(prediction=character(),
                         ngram=numeric(),
                         count=numeric(),
                         type=character())
    df <- bigrams[bigrams$first_word == last_word,]
    df2 <- bigrams[startsWith(bigrams$second_word,last_word) & bigrams$second_word != last_word,]
    if (nrow(df) > 0){
        output1 <- data.frame(prediction = paste0(' ',df$second_word), 
                             ngram = rep(2,length(df$count)), 
                             count = df$count,
                             type = rep('word', length(df$count)))
        output <- rbind(output,output1)
    }
    if (nrow(df2) > 0){
        summary <- df2 %>% group_by(second_word) %>% summarize(total = max(count)) %>% arrange(desc(total))
        summary <- summary[1:min(3,nrow(summary)),]
        output2 <- data.frame(prediction = summary$second_word,
                              ngram = rep(2, nrow(summary)),
                              count = summary$total,
                              type = rep('completion', nrow(summary)))
        output <- rbind(output,output2)
    }
    return(output)
}

unigram_prediction <- function(last_word){
    output <- data.frame(prediction=character(),
                         ngram=numeric(),
                         count=numeric(),
                         type=character())
    df2 <- unigrams[startsWith(unigrams$first_word,last_word) & unigrams$first_word != last_word,]
    if (nrow(df2) > 0){
        summary <- df2 %>% group_by(first_word) %>% summarize(total = max(count)) %>% arrange(desc(total))
        summary <- summary[1:min(3,nrow(summary)),]
        output2 <- data.frame(prediction = summary$first_word,
                              ngram = rep(1, nrow(summary)),
                              count = summary$total,
                              type = rep('completion', nrow(summary)))
        output <- rbind(output,output2)
    }
    return(output)
}


ngram_predict <- function(raw_input, alpha=0.04, beta=0){
    input <- clean_input(raw_input)
    num_words <- length(words(input))
    if (num_words <= 3){
        input <- paste('<s>', input)
        num_words <- num_words + 1
    }
    options <- data.frame(prediction=character(),
                          ngram=numeric(),
                          count=numeric(),
                          type=character())

    if (num_words >= 1){
        last_word <- words(input)[num_words]
        if (num_words >= 2){
            second_last_word <- words(input)[num_words-1]
            if (num_words >= 3){
                third_last_word <- words(input)[num_words-2]
                if (num_words >= 4){
                    fourth_last_word <- words(input)[num_words-3]
                    output_5 <- quintigram_prediction(last_word,
                                                    second_last_word,
                                                    third_last_word,
                                                    fourth_last_word)
                }
                output_4 <- quadrigram_prediction(last_word, 
                                                second_last_word, 
                                                third_last_word)
                options <- rbind(options, output_4)
            }
            output_3 <- trigram_prediction(last_word,
                                         second_last_word)
            options <- rbind(options, output_3)
        }
        output_2 <- bigram_prediction(last_word)
        options <- rbind(options, output_2)
        output_1 <- unigram_prediction(last_word)
        options <- rbind(options, output_1)
    }
    if (nrow(options) > 0){
        options$score <- (alpha ^ (max(options$ngram) - options$ngram)) * options$count * ( 1 + beta * (options$type=='completion'))
        options <- options[order(-options$score),]
        predictions <- unique(options[,1])[1:min(3, length(unique(options[,1])))]
        for (i in 1:length(predictions)){
            if (predictions[i]==' i'){
                predictions[i] = ' I'
            }
        }
    }
    else{
        predictions <- NULL
    }
    return(predictions)
}
