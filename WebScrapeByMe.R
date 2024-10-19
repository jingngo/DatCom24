list.of.packages <- c("stringr", "RCurl","plyr","XML","devtools", "tidyverse",
                      "modeltools", 'rtimes', "qdap", "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Package "rtimes" Development version from GitHub
devtools::install_github("ropengov/rtimes")

## load packages
library(tidyverse)
library(rtimes)
library(RCurl)
library(XML)
library(stringr)
library(plyr)

install.packages("pacman")
install.packages("qdap")

require(httr)
require(jsonlite)



base_url <- "https://openapi.naver.com/v1/search/news.json?"
search_term <- URLencode("query=데이팅") ## you can change the query term for something else
search_condition <- "&display=50&start=1&sort=date"
# add all above elements
naver_search <- paste0(base_url, search_term, search_condition)
# add our access info
clientId <- "xJSkHBGQoolfzs8tHbOR"
clientSecret <- "_m2Swy5ebd"
# call API by combining everything
naver_result <- httr::GET(naver_search,
                           add_headers("X-Naver-Client-Id" = clientId,
                                       "X-Naver-Client-Secret" = clientSecret))

# check if API is working correctly, the result = 200, congrats!
naver_result$status_code
naver_result$content

# convert unstructured response to usable data
api_char <- base::rawToChar(naver_result$content)

# if it finds data on the web that is dataframe, convert it to df
api_result <- jsonlite::fromJSON(api_char, flatten = TRUE)
naver_result <-
  naver_result$content %>%
  rawToChar %>%
  fromJSON %>% as.data.frame
View(n)

naver_result %>% 
  colnames() <- c("new_name1", "new_name2", "new_name3")

copy_result <- naver_result
df <- naver_result
list_col <- colnames(df)

#naver_result["items.title"] <- copy_result["items.title"]

# change column names
colnames(df) <- c(list_col[-5:-9], "title", "originallink", "link", "description", "pubDate")
colnames(df)


# clean the dataframe, either | => removing the occurences of both of them
df <- df %>%
  mutate(
    title = gsub("&quot|<b>|</b>", "", title),
    description = gsub("&quot|<b>|</b>", "", description)
  )



## NYT PROJECT
## Before using this functions, you need to have your own "API key" (which is FREE!)
## Please go to "http://developer.nytimes.com/apps/register" and create your own

Sys.setenv(NYTIMES_API_KEY = "fiR6cISQ5OHX2bASGHhZZ13Su5nDijWL") 
Sys.getenv("NYTIMES_API_KEY")

## Now, we'll search for some articles of interest using this article search API.
## The basic functionality is provided by a function called "as_search"
## Let's look at the function description first
require(rtimes)
?as_search

## Let's first do some basic search, with the term "Biden"
Trump.articles <- as_search(q = "Trump", begin_date = "20240101", end_date = '20241017')
Trump.articles


# str = structure, see what is stored in the list "Trump.articles"
str(Trump.articles, 1)

# access the table that contains info about articles
view(Trump.articles[[3]])
dat <- Trump.articles[[3]]
dat$abstract[1]

## copyright and meta-information
Trump.articles$copyright
Trump.articles$meta

## look at the data
Biden.articles$data
## Or, directly view the data.frame object
View(Biden.articles$data)

## By default, the search only returns first 10 hits.
## You can get the next 10 results by adding "page" option
## (e.g., page = 0 means first 10 hits, page = 1 means next 10 hits)
Trump.articles2 <- as_search(q = "Trump", begin_date = "20241001", end_date = '20241017',  all_results = TRUE)
Biden.articles2$data

## To get the entire result, add "all_results = TRUE" option (THIS MAY TAKE A LONG TIME!!)
## Biden.articles <- as_search(q = "Biden", begin_date = "20221201", end_date = '20230101', page = 2, all_results = TRUE)

## notice here that the results actually do not contain full text of the article.
## we try get a full text of each article by automatically retrieve / parse, and process each of the urls
## first we try to get 50 results from the search
## loop from page x = 0 to page x = 4, 10 results per batch >> 10*5 = 50
Trump.articles.first.50 <- lapply(0:4, function(x) {
  fraction <- as_search(q = "Trump", begin_date = "20241001", end_date = '20241017', page = x)
  Sys.sleep(2)
  fraction
})

str(Trump.articles.first.50, 1)
# call out 1 of the batches
Trump.articles.first.50[[5]][["data"]]
Trump.articles.first.50[[4]][["data"]]
Trump.articles.first.50[[3]][["data"]]
Trump.articles.first.50[[2]][["data"]]
Trump.articles.first.50[[1]][["data"]]

## create "mydata" object from the results
## mydata <- Biden.articles.first.50[[1]]
## mydata$meta$offset <- 50
mydata <- do.call(dplyr::bind_rows, 
                  lapply(1:5, function(x) Trump.articles.first.50[[x]]$data))

# check the dimension of our dataframe
dim(mydata)
mydata$abstract[1:5]
mydata$web_url

# CONTENT EXTRACTION, the previous step is just to make the df
require(RCurl)
require(XML)
require(stringr)

## let's do some initial check using our "mydata"
url <- getURL(mydata$web_url[1], .encoding = "UTF-8", .mapUnicode = T)
parsed_url <- htmlParse(url, encoding = "UTF-8")


# extract paragraph
plain.text <- xpathSApply(parsed_url, "//p", xmlValue) 
plain.text <- str_replace_all(plain.text, "[\r\n]" , "") ## remove line breaks
plain.text <- str_squish(plain.text) ## remove excessive white spaces
plain.text

## those are the sentences that we don't want to be included..
excluding.sentences <- c("", "Advertisement", "Supported by", "Send any friend a story",
                         "As a subscriber, you have 10 gift articles to give each month. Anyone can read what you share.")
plain.text <- plain.text[(!plain.text %in% excluding.sentences)]
cat(paste(plain.text[-1], collapse = " "))
plain.text

## try making a function that can wrap all of this 


## Given this reasonably works well, we do the same thing for all of the retrieved urls:
mydata <-
  mydata[!(grepl("https://www.nytimes.com/video/", mydata$web_url)),]

require(parallel)
mydata$full_text <- unlist(
  
  lapply(mydata$web_url, function(x) {
    url <- getURL(x, .encoding = "UTF-8", .mapUnicode = T)
    parsed_url <- htmlParse(url, encoding = "UTF-8")
    plain.text <- xpathSApply(parsed_url, "//p", xmlValue) ## extract all <p>***</p>
    plain.text <- str_replace_all(plain.text, "[\r\n]" , "") ## remove line breaks
    plain.text <- str_squish(plain.text)
    plain.text <- plain.text[(!plain.text %in% excluding.sentences)]
    full_text <- paste(plain.text, collapse = " ")
    full_text
  })
)


## check the results by viewing the data frame
View(mydata)
mydata$full_text[1]





