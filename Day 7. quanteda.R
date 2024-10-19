
## basic dictionary methods using Quanteda
install.packages("quanteda")
install.packages("devtools")
devtools::install_github("quanteda/quanteda.corpora")

list.of.packages <- c("stringr", "RCurl", "plyr", "tm", "SentimentAnalysis",
                      "tidytext", 'tibble', "readtext")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

require(quanteda, warn.conflicts = FALSE, quietly = TRUE)

require(stringr)
require(readtext)
require(RCurl)
library(plyr)
library(tm)

Sys.setlocale("LC_ALL","English") # If you're using mac, you'll encounter an error message. don't worry..

?tokens
# create stop word list
stopWordlist <- quanteda::stopwords("english") # define "stop-words" from English stopword lists..

# tokenize the text -> "A", "corpus", ..., other args are helpful: remove_numbers, remove_symbols
toks <- tokens(c("A corpus is a set of documents",
                 "This is the second document in the corpus"),
               remove_punct = T)
toks
# stem (v): remove the grammar, now "document" don't have "s" anymore
tokens_wordstem(toks)



# next class: start from here
tokens_wordstem(toks) %>%
  tokens_remove(stopwords("english"))

tokens_wordstem(toks) %>%
  tokens_remove(stopwords("english")) %>%
  types


corp <- corpus(c("A corpus is a set of documents",
                 "This is the second document in the corpus"))

summary(corp)

corp %>%
  tokens(remove_punct = T) %>%
  tokens_wordstem %>%
  tokens_remove(stopwords("english")) %>%
  dfm

dfm(
  tokens_remove(
    tokens_wordstem(
      tokens(corp, remove_punct = T)),
    stopwords("english")))



## working with real data
load(file.choose()) ## select and load mydata.Rdata file you downloaded!
mydata <- cbind(doc_id = seq_len(nrow(mydata)),
                mydata)

mydata_corpus <- corpus(mydata,
                        docid_field = "doc_id",
                        text_field = "full_text")
head(docvars(mydata_corpus))

dfm_mydata_corpus <-
  mydata_corpus %>%
  tokens(remove_punct = T,
         remove_symbols = T) %>%
  tokens_wordstem %>%
  tokens_remove(stopwords("english")) %>%
  dfm

## most frequent features
topfeatures(dfm_mydata_corpus, n = 50)

## trimming dfm
dfm_trim(dfm_mydata_corpus,
         min_termfreq = 10)

dfm_trim(dfm_mydata_corpus,
         min_docfreq = 5)

dfm_trim(dfm_mydata_corpus,
         min_docfreq = 0.1,
         docfreq_type = "prop")


## sentiment analysis
summary(data_dictionary_LSD2015)

## tokenize and apply dictionary
toks_dict <-
  mydata_corpus %>%
  tokens() %>%
  tokens_lookup(dictionary = data_dictionary_LSD2015)

## transform to a dfm
dfm_mydata_LSD2015 <- dfm(toks_dict)


## convert and estimate sentiment
dict_output <- convert(dfm_mydata_LSD2015, to = "data.frame")
dict_output$sent_score <- log(
                              (dict_output[,3] + dict_output[,5] + 0.5) /
                              (dict_output[,2] + dict_output[,4] + 0.5)
                              )
dict_output <- cbind(dict_output, docvars(mydata_corpus))


dict_output$section_name <-
  factor(dict_output$section_name,
         levels = c("U.S.", "Opinion", 
                    "Climate", "Business Day", 
                    "Style", 
                    "The Upshot"))

ggplot(data) +
  geom_bar( aes(x=name, y=value), 
            stat="identity", 
            fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=name, 
                     ymin=value-sd, 
                     ymax=value+sd), 
                 width=0.4, colour="orange", 
                 alpha=0.9, size=1.3)

## ggplot

dict_output %>%
  group_by(section_name) %>%
  summarize(mean_sent_score = mean(sent_score, na.rm = T),
            sd_mean_sent_score = sd(sent_score, na.rm = T)) %>%
  ggplot(aes(x = section_name, 
             y = mean_sent_score, 
             fill = section_name)) + 
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_sent_score - sd_mean_sent_score,
                    ymax = mean_sent_score + sd_mean_sent_score), 
                width = 0.2, size = 0.5) + 
  theme_bw()




## writing functions to calculate sentiment per sentence
# define dictionary (a search term input) to count visibility of the actors

## function process.actor.mention.count (counts the number of appearance of each party name)
## and compute valence of each sentence that contains the actor

load(file.choose()) ## select and load mydata.Rdata file you downloaded!
require(quanteda)
require(data.table)

actors <- c("Biden", "Trump", "Democrats", "Republicans")
actor <- "Biden"
full_text <- mydata$full_text[[1]]

actor.mention.valence <- function(actor, full_text) {

  require(tidyverse)
  require(quanteda)
  require(data.table)

  ## sentence location of actor mention within text
  loc_docs <-
    full_text %>%
    gsub("Mr. ","", .) %>%
    corpus %>%
    corpus_segment(pattern = ".", valuetype = "fixed",
                   pattern_position = "after",
                   extract_pattern = FALSE) %>%
    grep(actor, .)

  sent_docs <-
    full_text %>%
    gsub("Mr. ","", .) %>%
    corpus %>%
    corpus_segment(pattern = ".", valuetype = "fixed",
                   pattern_position = "after",
                   extract_pattern = FALSE) %>%
    .[loc_docs]

  ## only extract sentences that mention actor
  sent_docs <- sent_docs %>%
    tokens(remove_punct = T,
           remove_symbols = T) %>%
    tokens_tolower %>%
    tokens_wordstem %>%
    tokens_remove(stopwords("english")) %>%
    tokens_lookup(dictionary = data_dictionary_LSD2015) %>%
    dfm %>%
    convert(to = "data.frame") %>%
    setDT

  ## calculate net valence
  sent_docs[, sent_score :=
              log( (positive + neg_negative + 0.5) /
                     (negative + neg_positive + 0.5)
              )]

  ## times of actor mention and net valence
  mention_times <- dim(sent_docs)[1]
  names(mention_times) <- paste0(actor, ".mention.N")
  sent_score <- sent_docs[, mean(sent_score, na.rm = T)]

  return(c(mention_times,
           sent_score = sent_score))
}

## check function if it works well
actor.mention.valence("Biden", mydata$full_text[1])
actor.mention.valence("Biden", mydata$data$full_text[[2]])

test <-
  lapply(1:4, function(i) {
  lapply(1:43, function(x) {
    actor.mention.valence(actors[i], mydata$data$full_text[[x]])
  })
})

data.frame(do.call(rbind, test[[1]]),
           do.call(rbind, test[[2]]),
           do.call(rbind, test[[3]]),
           do.call(rbind, test[[4]]))


# below function performs actual analysis
require(parallel)
processTxt <- function(dat) {

  temp <-
    mclapply(actors, function(x) {
      scores <- lapply(dat$full_text,
                       function(y) actor.mention.valence(x, y))
      scores <- do.call(rbind, scores)
      return(scores)
    }, mc.cores = length(actors))

  temp <- do.call(cbind, temp)
  temp <- as.data.frame(temp)
  return(temp)
}

processTxt_win <- function(dat) {

  cl <- makeCluster(getOption("cl.cores", length(actors)))
  clusterExport(cl = cl, varlist = c("dat", "actors", "actor.mention.valence"), envir = environment())
  temp <-
    parLapply(cl, actors, function(x) {
      scores <- lapply(dat$full_text,
                       function(y) actor.mention.valence(x, y))
      scores <- do.call(rbind, scores)
      return(scores)
    })
  temp <- do.call(cbind, temp)
  temp <- as.data.frame(temp)
  return(temp)
}



#-----------------------------------------------------------

# let's process data ....
require(parallel)
mydata.sentiment <- processTxt(mydata) ## osx
## mydata.sentiment <- processTxt_win(mydata) ## windows

require(data.table)
# merge the dataset
setDT(mydata)
setDT(mydata.sentiment)

mydata <- cbind(mydata, mydata.sentiment)
View(mydata)


## cf. parallel processing using mapply-type call
index <- expand.grid(1:length(actors), 1:length(mydata$full_text))
test <- mcmapply(actor.mention.valence, actors[index$Var1], mydata$full_text[index$Var2])
