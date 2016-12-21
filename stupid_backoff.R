############################ INIT #################################
require(openssl,quietly = TRUE) # ms5
require(DBI,quietly = TRUE)     # sqlite

# container to pack app methods,
# avoiding to pollute the global
# namespace
nxtWord = list();

# SQLite database name
nxtWord$dbname = "ngrams.sqlite"

# Helper function for markov_rec
# checks whether there are 3 results,
# if the there are less than three
# we will complete the result set
# with most frequent unigrams
nxtWord$complete_results <- function(results,last){
    # make sure we have at least 3 results
    nresults <- nrow(results)
    if(nresults<3){
        # there are less than 3 resuls, complete with 3 top unigrams
        ninsert <- 3 - nresults # number of results missing to complete up to 3
        # get 'ninsert' unigrams not in the predictions set (results$last)
        q <- !(nxtWord$unigrams$last %in% results$last) & !(nxtWord$unigrams$last %in% last)
        inserts <- nxtWord$unigrams[q,][1:ninsert,]
        # format these unigrams as a data frame with the same structure as
        # the result set
        inserts <- data.frame(
            prefix = rep(results$prefix[1],ninsert),
            last = inserts$last,
            freq = inserts$freq,
            n = 1,
            stringsAsFactors = FALSE,
            row.names = NULL
        )
        # bind both data frames
        results <- rbind(results,inserts)
    }
    results
}

###################################################################
# Markov Chain
# this recursive algorithm looks for ngrams
# with the same prefix as the user input 
# and will return the recorded last words
# along with their counts in the training set
# when the prefix is not found, the first word
# in the prefix is dropped and the ramaining 
# prefix is tested (recursion) 
nxtWord$markov_rec <- function(words){
    # number of words
    nwords <- length(words)
    # we go up to quadrigrams
    if(nwords > 3){
        # look up fourgrams
        start <- nwords-2
        words = words[start:nwords]
        nwords <- length(words)
    }
    # get all ngrams of the of degree n+1 (n being the number of input words),
    # where these words are a prefix for these (n+1)-grams.
    prefix_md5 = md5(paste(c(words),collapse = " "))
    q <- "SELECT prefix,last,freq,n FROM ngrams"
    q <- paste0(q," WHERE n=",(nwords+1)," AND prefix_md5=X'")
    q <- paste0(q,prefix_md5,"'")
    results <- nxtWord$get_query_results(q)
    
    if(dim(results)[1] == 0){
        # nothing found 
        if(nwords <= 1){
            # nothing found and no more words left
            results <- nxtWord$unigrams
        }else{
            # recursion
            return(nxtWord$markov_rec(words = words[-1]))
        }
    }
    nxtWord$complete_results(results,words[length(words)])
}

###################################################################
# Wraper to run queries in SQLite
nxtWord$get_query_results <- function(query){
    mydb <- dbConnect(RSQLite::SQLite(),nxtWord$dbname)
    results <- dbGetQuery(mydb, query)
    dbDisconnect(mydb)
    results
}

###################################################################
# Helper function to load unigrams
nxtWord$get_unigrams <- function(limit=100){
    q <- "SELECT last,freq FROM ngrams WHERE n=1 ORDER BY freq DESC"
    if(is.integer(limit) && limit>0){
        q <- sprintf("%s LIMIT %d",q,limit)   
    }
    nxtWord$get_query_results(q)
}

# load unigrams into environment
nxtWord$unigrams <- nxtWord$get_unigrams(0);

###################################################################
# Helper function for Stupid Backoff implementation
# f(w_{i-k+1},...,w_{i-1},w_{i}) Numerator
# f(w_{i-k+1},...,w_{i-1}) Denominator
nxtWord$get_first_term <- function(prefix,last="",what="numerator"){
    if(what=="denominator"){
        words <- strsplit(prefix," ",FALSE)[[1]]
        if(length(words)>1){
            prefix <- paste(words[-length(words)],collapse = " ")
            last <- paste(words[length(words)])
        }else{
            # unigram
            last <- prefix
        }
    }
    q <- sprintf("SELECT CASE WHEN COUNT(1) > 0 THEN freq ELSE 0 END AS freq 
                 FROM ngrams WHERE prefix_md5=X'%s' AND last='%s';",
                 openssl::md5(prefix),gsub("'","''",last))
    
    nxtWord$get_query_results(q)$freq
}

###################################################################
# Stupid backoff implementation
# See: Large Language Models in Machine Translation
# http://www.aclweb.org/anthology/D07-1090.pdf
nxtWord$S <- function(last,prefix,alpha=0.4){
    if(identical(prefix,character(0)) || prefix=="" || identical(prefix,character(0))){
        # recursion ends at unigrams
        return(nxtWord$unigrams[nxtWord$unigrams$last == last,"freq"] / nrow(nxtWord$unigrams))
    }
    numerator <- nxtWord$get_first_term(prefix = prefix,last = last,what="numerator")
    denominator <- nxtWord$get_first_term(prefix,last,what = "denominator")
    if(numerator > 0 && denominator > 0){
        numerator / denominator
    }else{
        prefix = strsplit(prefix," ")[[1]][-1]
        if(length(prefix)>1){
            prefix <- paste(prefix,collapse = ' ')
        }
        return(alpha*nxtWord$S(last,prefix,alpha))
    }
}

nxtWord$predict <- function(prefix){
    prefix <- as.character(prefix)
    words <- tolower(strsplit(x = prefix,split = " ",fixed = TRUE)[[1]])
    words <- gsub("[^a-z']","",words)
    words <- words[words != ""]
    candidates <- nxtWord$markov_rec(words = words)
    predictions <- sapply(candidates$last[1:min(10,nrow(candidates))],nxtWord$S,prefix=prefix)
    predictions <- unlist(predictions)
    predictions <- predictions[order(predictions,decreasing = TRUE)][1:3]
    pred_words <- names(predictions)
    pred_words <- gsub("^i$","I", pred_words)
    pred_words <- gsub("^i'm$","I'm",pred_words)
    data.frame(
        prediction = pred_words,
        score = round(predictions,4),
        stringsAsFactors = FALSE,
        row.names = NULL
    )
}