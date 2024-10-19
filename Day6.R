
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
library(qdap)

require(httr)

## set the working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

clientId <- "7Xld6GXWM7Dq1CakRAlP"
clientSecret <- "zM6pIWQbBH"

## Naver news search using open api

base_url <- "https://openapi.naver.com/v1/search/news.json?"
search_term <- URLencode("query=오물풍선") ## you can change the query term for something else
search_condition <- "&display=50&start=1&sort=date"
naver_search <- paste0(base_url, search_term, search_condition)

naver_results <- httr::GET(naver_search,
                           add_headers("X-Naver-Client-Id" = clientId,
                                       "X-Naver-Client-Secret" = clientSecret))

require("jsonlite")
naver_search.results <-
  naver_results$content %>%
  rawToChar %>%
  fromJSON %>% as.data.frame
View(n)

# start cleaning
naver_search.results$items.title <-
  gsub("&quot;","", naver_search.results$items.title)
naver_search.results$items.title <-
  gsub("<b>", "", naver_search.results$items.title)
naver_search.results$items.title <-
  gsub("</b>", "", naver_search.results$items.title)


naver_search.results$items.description <-
  gsub("&quot;", '', naver_search.results$items.title)
naver_search.results$items.description <-
  gsub("<b>", "", naver_search.results$items.title)
naver_search.results$items.description <-
  gsub("</b>", "", naver_search.results$items.title)


## Before using this functions, you need to have your own "API key" (which is FREE!)
## Please go to "http://developer.nytimes.com/apps/register" and create your own

Sys.setenv(NYTIMES_API_KEY = "lhZ6NM8bL6Xh251t0oTM0lWhh5NpdMYK") ## 4VYDWWlGGuEwKM9DzrMASGC5XixSPV4A


## Now, we'll search for some articles of interest using this article search API.
## The basic functionality is provided by a function called "as_search"
## Let's look at the function description first
require(rtimes)
?as_search

## Let's first do some basic search, with the term "Biden"
Biden.articles <- as_search(q = "Biden", begin_date = "20240701", end_date = '20240704')

## copyright and meta-information
Biden.articles$copyright
Biden.articles$meta

## look at the data
Biden.articles$data
## Or, directly view the data.frame object
View(Biden.articles$data)

## By default, the search only returns first 10 hits.
## You can get the next 10 results by adding "page" option
## (e.g., page = 0 means first 10 hits, page = 1 means next 10 hits)
Biden.articles2 <- as_search(q = "Biden", begin_date = "20240701", end_date = '20240704', page = 1)
Biden.articles2$data

## To get the entire result, add "all_results = TRUE" option (THIS MAY TAKE A LONG TIME!!)
## Biden.articles <- as_search(q = "Biden", begin_date = "20221201", end_date = '20230101', page = 2, all_results = TRUE)

## notice here that the results actually do not contain full text of the article.
## we try get a full text of each article by automatically retrieve / parse, and process each of the urls
## first we try to get 50 results from the search
Biden.articles.first.50 <- lapply(0:4, function(x) {
  fraction <- as_search(q = "Biden", begin_date = "20240701", end_date = '20240704', page = x)
  Sys.sleep(2)
  fraction
})

## create "mydata" object from the results
## mydata <- Biden.articles.first.50[[1]]
## mydata$meta$offset <- 50
mydata <- do.call(dplyr::bind_rows, 
                  lapply(1:5, function(x) Biden.articles.first.50[[x]]$data))


require(RCurl)
require(XML)
require(stringr)
## let's do some initial check using our "mydata"
url <- getURL(mydata$web_url[1], .encoding = "UTF-8", .mapUnicode = T)
parsed_url <- htmlParse(url, encoding = "UTF-8")
plain.text <- xpathSApply(parsed_url, "//p", xmlValue) ## extract all <p>***</p>
plain.text <- str_replace_all(plain.text, "[\r\n]" , "") ## remove line breaks
plain.text <- str_squish(plain.text) ## remove excessive white spaces

## those are the sentences that we don't want to be included..
excluding.sentences <- c("", "Advertisement", "Supported by", "Send any friend a story",
                         "As a subscriber, you have 10 gift articles to give each month. Anyone can read what you share.")
plain.text <- plain.text[(!plain.text %in% excluding.sentences)]
cat(paste(plain.text[-1], collapse = " "))

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
    full_text <- paste(plain.text[-1], collapse = " ")
    full_text
})
)

## check the results by viewing the data frame
View(mydata)



