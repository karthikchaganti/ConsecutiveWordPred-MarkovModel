############## CSE 674 Project 1 and 2 Combined Code ##############
## Authors: 
## 1. KARTHIK CHAGANTI :  kchagant :50169441
## 2. VAIBHAV LELLA    :  vaibhavl :50169859
###################################################################

# Tokenise
itoken <- function(x)
{
        x <- tolower(x)
        x <- removeNumbers(x)
        x <- removePunctuation(x)
        x <- gsub("[^[:print:]]","", x)
        #x <- stemDocument(x)
        x <- stripWhitespace(x)
        
        a <- matrix(unlist(strsplit(x, " ")))
        
        return(a)
}

# Finding the last howmany words of a given phrase
lastwords <- function(x, howmany)
{
        a <- matrix(x[(length(x)-howmany+1):length(x)]) 
        a <- apply(a, MARGIN = 2, FUN = function(x){ paste(x, collapse=" ")})
        
        return(a)
}


# Search
findpartial <- function(x, dt, weight)
{
        dttemp <- dt[x] 
        dttemp <- dttemp[order(dttemp$freq, decreasing=TRUE),][1:4]
        dttemp$weighted <- dttemp$freq*weight
        setkey(dttemp, prefix)
        dttemp <- dttemp[x]
        dttemp
}


# Main function
findanswer <- function(x, dt1, dt2, dt3)
{
        tokens <- itoken(x)
        weights <- c(0.47, 0.31, 0.21, 0.01)
        needs <- 4
        answertable <- data.table(prefix=NA, answer=NA, freq=NA, weighted=NA)
        
        # Examining the Quad-gram table
        if (length(tokens) > 2)
        {token <- lastwords(tokens, 3)
        answertable <- findpartial(token, dt1, weights[1])
        if (!is.na(answertable[1,1, with=FALSE])){needs <- 4-nrow(answertable)}
        }
        
        # Examining the Tri-gram table
        if ((needs > 0) & (length(tokens) > 1))
        {token <- lastwords(tokens, 2)
        partial <- findpartial(token, dt2, weights[2])
        if (!is.na(answertable[1,1, with=FALSE])){answertable <- rbind(answertable, partial)}
        else {answertable <- partial}
        if (!is.na(answertable[1,1, with=FALSE])){needs <- 4-nrow(answertable)}
        }
        
        # Examining the Bi-gram table
        if (needs > 0 & (length(tokens) > 0))
        {token <- tokens[length(tokens)]
        partial <- findpartial(token, dt3, weights[3])
        if (!is.na(answertable[1,1, with=FALSE])){answertable <- rbind(answertable, partial)}
        else {answertable <- partial}
        if (!is.na(answertable[1,1, with=FALSE])){needs <- 4-nrow(answertable)}
        }
        
        # Adding a pro-forma answer
        if ((needs > 0) & (length(tokens) > 0))
        {
                token <- tokens[length(tokens)]
                if (!is.na(answertable[1,1, with=FALSE])){answertable <- rbind(answertable, data.frame(prefix=token, answer="that", freq=1, weighted=weights[4]))}     
                else  
                {answertable <- data.table(prefix=token, answer="that", freq=1, weighted=weights[4])
                }
        }
        
        # Aggregating the results
        answertable2 <- answertable[, sum(weighted), by=answer]
        setnames(answertable2, c("answer", "confidence (%)"))
        answertable2[,2] <- 100* answertable2$confidence/sum(answertable2$confidence)
        
        return(answertable2)
}